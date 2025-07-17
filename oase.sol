// SPDX-License-Identifier: UNLICENSED
// All Rights Reserved
pragma solidity ^0.8.20;

import "npm/@openzeppelin/contracts@v4.9.6/token/ERC20/ERC20.sol";
import "npm/@openzeppelin/contracts@v5.0.0/utils/cryptography/MerkleProof.sol";
import "npm/@openzeppelin/contracts@v5.0.0/utils/cryptography/ECDSA.sol";
import "npm/@openzeppelin/contracts@v5.0.0/utils/cryptography/MessageHashUtils.sol";

/**
 * @title Oase
 * @dev A savings token contract that provides a single saving/locking feature. The lock length is fixed at 369 days,
 *      and there is one global share rate that is recalculated daily.
 *
 *      The daily share rate growth is determined by a formula that depends on the ratio of the
 *      current token value of all locked shares to the conceptual total supply (circulating supply
 *      plus the current token value of all locked shares).
 *      For example, if 100% of the conceptual total supply (by value) is locked, the target annual rate
 *      for share growth is approximately 3.69%. If only 10% (by value) is locked, it's approximately 36.9%,
 *      and if 1% (by value) is locked, it's approximately 369%, and so on.
 *
 *      NOTE: All percentage references are annual. Daily growth is approximated by dividing the annual growth parameter by 365.
 *      This means the actual compounded Annual Percentage Rate (APR) is an approximation.
 */
contract Oase is ERC20 {
    // ------------------------------------------------------------------------
    // Global saving feature constants
    // ------------------------------------------------------------------------

    /**
     * @dev Single, fixed lock length in days (369 days).
     */
    uint256 public constant LOCK_LENGTH_DAYS = 369;

    /**
     * @dev Maximum number of days to process in a single _dailyDataUpdate call.
     */
    uint256 private constant MAX_DAYS_TO_UPDATE = 369;

    /**
     * @dev Start of contract in Unix time. For demonstration, set to block.timestamp 
     *      at deployment.
     */
    uint256 public immutable LAUNCH_TIME;

    // ------------------------------------------------------------------------
    // Global saving system variables
    // ------------------------------------------------------------------------

    /**
     * @dev Stores global variables for the contract's saving feature.
     *      It tracks the current shareRate, the number of days fully processed by dailyDataUpdate,
     *      the total amount of tokens initially deposited into active savings, and the total number of shares
     *      currently locked across all savings.
     */
    struct Globals {
        uint256 shareRate;         // Current share rate (scaled by 1e8)
        uint256 dailyDataCount;    // Number of days fully processed by dailyDataUpdate
        uint256 lockedTokenSupply; // Total tokens initially deposited and still locked
        uint256 totalSharesLocked; // Total shares currently locked across all savings
    }

    /**
     * @dev A copy of Globals in memory for calculations in each function call,
     *      to avoid repeated storage reads/writes.
     */
    struct GlobalsCache {
        uint256 _shareRate;
        uint256 _dailyDataCount;
        uint256 _lockedTokenSupply; // Total tokens initially deposited and still locked
        uint256 _totalSharesLocked; // Total shares currently locked
        uint256 _currentDay;       // We also keep track of the "current day" in memory
    }

    Globals public globals; // The single instance of our Globals struct in storage

    // ------------------------------------------------------------------------
    // Structures / Storage for daily share rate data
    // ------------------------------------------------------------------------
    struct DailyDataStore {
        uint256 dayShareRate;         // records what the shareRate was after that day's update
    }

    mapping(uint256 => DailyDataStore) public dailyData;

    // ------------------------------------------------------------------------
    // Structures / Storage for each saving
    // ------------------------------------------------------------------------
    struct SavingStore {
        uint40 savingId;           // Unique ID for this saving
        uint256 savedShares;       // Number of shares purchased at start
        uint256 savedTokens;       // Number of tokens burned at start
        uint256 savingStartDay;    // Day the saving starts
        uint256 savingEndDay;      // Day the saving can be ended
    }

    /**
     * @dev We keep a unique ID for each new saving across all addresses
     */
    uint40 public latestSavingId;

    /**
     * @dev Each user can have multiple savings
     */
    mapping(address => SavingStore[]) public savingsLists;

    /**
     * @dev Merkle root for verification of claim eligibility 
     */
    bytes32 public immutable merkleRoot;

    /**
     * @dev Tracks whether a claim ID has been used
     */
    mapping(uint256 => bool) public hasClaimedId;

    // ------------------------------------------------------------------------
    // Events
    // ------------------------------------------------------------------------
    event DailyDataUpdate(
        uint256 indexed timestamp,
        uint256 indexed currentDay,
        uint256 newShareRate
    );

    event SavingStart(
        uint256 indexed data,         // Packed data of (block.timestamp, savedTokens, savedShares)
        address indexed saverAddr,
        uint40 indexed savingId
    );

    event SavingEnd(
        uint256 indexed data,         // Packed data of (block.timestamp, savedShares, payoutTokens)
        address indexed saverAddr,
        uint40 indexed savingId
    );

    /**
     * @dev Emitted when a claim is successfully processed
     */
    event Claimed(
        uint256 indexed id,
        address indexed account, 
        uint256 amount, 
        uint256 timestamp
    );



    // ------------------------------------------------------------------------
    // Constructor
    // ------------------------------------------------------------------------
    constructor(bytes32 _merkleRoot) 
        ERC20("Oase", "OASE")  // Name and Symbol via OpenZeppelin's constructor
    {
        merkleRoot = _merkleRoot;
        LAUNCH_TIME = block.timestamp;

        // Initialize our persistent storage
        globals.shareRate = 1e8;       // Start shareRate at 1.0 (scaled by 1e8)
        globals.dailyDataCount = 0;
        globals.lockedTokenSupply = 0; // Initialize total tokens locked
        globals.totalSharesLocked = 0; // Initialize new total shares locked
    }

    // ------------------------------------------------------------------------
    // ERC20 Overrides
    // ------------------------------------------------------------------------

    /**
     * @dev Returns the number of decimals used to get its user representation.
     * Overrides the default 18 decimals to use 8 decimals.
     */
    function decimals() public view virtual override returns (uint8) {
        return 8;
    }

    // ------------------------------------------------------------------------
    // GlobalsCache load/sync
    // ------------------------------------------------------------------------

    /**
     * @dev Load global values into a memory-based cache so we can manipulate them
     *      without constantly reading from storage.
     */
    function _globalsLoad(GlobalsCache memory g, GlobalsCache memory gSnapshot)
        internal
        view
    {
        g._shareRate         = globals.shareRate;
        g._dailyDataCount    = globals.dailyDataCount;
        g._lockedTokenSupply = globals.lockedTokenSupply; // Load total tokens locked
        g._totalSharesLocked = globals.totalSharesLocked; // Load total shares locked
        g._currentDay        = _currentDay();

        _globalsCacheSnapshot(g, gSnapshot);
    }

    /**
     * @dev Take a snapshot of the current memory-based GlobalsCache.
     *      We'll compare with this snapshot to see if anything changed
     *      before writing back to storage.
     */
    function _globalsCacheSnapshot(GlobalsCache memory g, GlobalsCache memory gSnapshot)
        private
        pure
    {
        gSnapshot._shareRate         = g._shareRate;
        gSnapshot._dailyDataCount    = g._dailyDataCount;
        gSnapshot._lockedTokenSupply = g._lockedTokenSupply; // Snapshot total tokens locked
        gSnapshot._totalSharesLocked = g._totalSharesLocked; // Snapshot total shares locked
        gSnapshot._currentDay        = g._currentDay;
    }

    /**
     * @dev Compare memory-based GlobalsCache to the snapshot. If anything changed,
     *      write the updated values back to storage. This saves gas by storing only
     *      if changes occurred.
     */
    function _globalsSync(GlobalsCache memory g, GlobalsCache memory gSnapshot)
        internal
    {
        // If any relevant field changed, update
        if (
            g._shareRate         != gSnapshot._shareRate
         || g._dailyDataCount    != gSnapshot._dailyDataCount
         || g._lockedTokenSupply != gSnapshot._lockedTokenSupply // Check total tokens locked
         || g._totalSharesLocked != gSnapshot._totalSharesLocked // Check total shares locked
        ) {
            globals.shareRate         = g._shareRate;
            globals.dailyDataCount    = g._dailyDataCount;
            globals.lockedTokenSupply = g._lockedTokenSupply; // Sync total tokens locked
            globals.totalSharesLocked = g._totalSharesLocked; // Sync total shares locked
        }
    }

    // ------------------------------------------------------------------------
    // Time / Day Utilities
    // ------------------------------------------------------------------------
    function _currentDay() internal view returns (uint256) {
        // Each "day" is a 24-hour period from LAUNCH_TIME
        return (block.timestamp - LAUNCH_TIME) / 1 days;
    }

    // ------------------------------------------------------------------------
    // Internal daily data update logic
    // ------------------------------------------------------------------------
    /**
     * @dev Processes daily updates to the share rate. It iterates day-by-day from the last fully processed day
     *      (`g._dailyDataCount`) up to (but not including) the target `beforeDay`. In each iteration for a `currentDay`,
     *      it first records the existing `g._shareRate` (representing the rate at the start of `currentDay + 1`)
     *      into `dailyData[currentDay + 1]`. Then, it calculates and applies the share rate growth for `currentDay`
     *      to `g._shareRate`. The loop runs for a maximum of MAX_DAYS_TO_UPDATE days per call.
     * @param g The GlobalsCache memory struct, containing current global values.
     * @param beforeDay The target day to process updates up to (exclusive).
     * @return completed True if the update process reached `beforeDay`, false otherwise (e.g., if MAX_DAYS_TO_UPDATE limit was hit).
     */
    function _dailyDataUpdate(GlobalsCache memory g, uint256 beforeDay) private returns (bool completed) {
        uint256 daysToProcess = beforeDay > g._dailyDataCount ? beforeDay - g._dailyDataCount : 0;

        if (daysToProcess == 0) {
            return true; // Already up-to-date
        }

        uint256 daysThisRun = daysToProcess > MAX_DAYS_TO_UPDATE ? MAX_DAYS_TO_UPDATE : daysToProcess;
        uint256 endDayForLoop = g._dailyDataCount + daysThisRun;

        // Process each day from dailyDataCount up to endDayForLoop
        for (uint256 currentDay = g._dailyDataCount; currentDay < endDayForLoop; currentDay++) {
            // 1) Record the current shareRate in dailyData
            dailyData[currentDay + 1] = DailyDataStore({
                dayShareRate: g._shareRate
            });

            // 2) Apply next day's rate adjustments
            _applyDailyRate(g);
        }

        // Update dailyDataCount to the day we processed up to
        g._dailyDataCount = endDayForLoop;

        // Emit a single event showing the final shareRate and day processed up to
        // Note: The event timestamp is current block, but day processed might be in the past
        emit DailyDataUpdate(block.timestamp, g._dailyDataCount, g._shareRate);

        // Return true only if we reached the target day
        return g._dailyDataCount == beforeDay;
    }

    /**
     * @dev Calculates and applies the share rate growth for a single day. The growth is determined by
     *      a ratio derived from the current token value of all locked shares relative to the conceptual total supply.
     */
    function _applyDailyRate(GlobalsCache memory g) internal view {
        // Get locked ratio in 1e8 scale
        uint256 ratio = _getLockedRatio(g);
        
        // If ratio is 0 (returned by _getLockedRatio if currentValueOfLockedShares is 0),
        // the share rate should not increase. It remains unchanged for this day.
        if (ratio == 0) {
            return; // No change to g._shareRate
        }
        
        // If ratio is non-zero, _getLockedRatio ensures it's at least 1e5 (0.1%).
        // Calculate target annual APR (in 1e8 scale)
        // 3.69% when ratio is 100%, 36.9% when ratio is 10%, 369% when ratio is 1%.
        // Min ratio of 1e5 (0.1%) means max APR from this formula is 36900%.
        uint256 targetAnnualPercent = (3690000 * 1e8) / ratio;
        
        // Calculate daily rate that will compound to the annual multiplier
        // Formula: dailyRate = (annualMultiplier)^(1/365) - 1

        // Note on Approximations, Achieved APY for Savers, and Total Supply Inflation:
        //
        // 1. Approximation Precision and Achieved APY for Savers:
        // The daily rate calculation uses approximations. These may become less precise at very
        // high targetAnnualPercent values (which occur when the locked token ratio is low).
        // In such scenarios, the approximation tends to be conservative (i.e., it underestimates
        // the required daily rate), potentially resulting in an actual compounded APY for savers
        // that is lower than the high targetAnnualPercent. This means that while a lower locked
        // ratio aims for a higher *targeted APY for savers*, the actually achieved APY might be
        // tempered by this approximation. Conversely, for lower targetAnnualPercent values
        // (e.g., at high locked ratios), the deviation between the actual APY for savers and
        // the targetAnnualPercent might differ (often being slightly higher due to the nature of the approximation).
        //
        // 2. Impact on Total Supply Inflation:
        // This interplay affects the overall total supply inflation (i.e., the total number of new tokens minted).
        // Extremely low locked ratios (e.g., <1%) target very high APYs for savers. However, the
        // actually achieved APY on this small base of locked tokens (potentially reduced further by the
        // approximation's conservatism at high targets) can result in fewer absolute new tokens minted
        // compared to scenarios with higher locked ratios.
        // Higher locked ratios (e.g., 10%-100%) apply a more moderate (and more accurately achieved) APY
        // for savers to a larger base of locked tokens. Consequently, the total supply inflation
        // (measured as new tokens minted as a percentage of the conceptual total supply) may actually
        // be higher when more tokens are locked. This total supply inflation might not significantly
        // exceed the ~3.69% annual rate (which is achieved when 100% of tokens are locked and earn
        // a ~3.69% APY for savers), even if APY targets for savers are drastically higher at very
        // low locked ratios.

        // Conservative approximation that works for our range:
        uint256 dailyRate;
        if (targetAnnualPercent <= 10e8) {  // <= 1000%
            // Use logarithmic approximation: ln(1+x) ≈ x/(1+x/2)
            dailyRate = (targetAnnualPercent * 1e8) / (1e8 + targetAnnualPercent / 2) / 365;
        } else {
            // For very high rates, use direct calculation with better approximation
            // This gives a more accurate daily rate for high annual percentages
            dailyRate = (targetAnnualPercent * 1e8) / (1e8 + targetAnnualPercent / 3) / 365;
        }
        
        // Apply the daily rate to the share rate
        g._shareRate = (g._shareRate * (1e8 + dailyRate)) / 1e8;
    }

    // ------------------------------------------------------------------------
    // Ratio / Rate Calculation Helpers
    // ------------------------------------------------------------------------

    /**
     * @dev Calculates the ratio of the current token value of all locked shares
     *      to the conceptual total supply (circulating supply + current token value of locked shares).
     *      The ratio is scaled by 1e8 (e.g., 1e8 represents a 1.0 ratio, 1e7 represents 0.1).
     */
    function _getLockedRatio(GlobalsCache memory g) internal view returns (uint256) {
        // Use totalSupply() from ERC20 (which is circulating supply)
        uint256 circulatingSupply = totalSupply();

        // Calculate the current "value" of all locked shares in terms of tokens
        uint256 currentValueOfLockedShares;
        if (g._totalSharesLocked == 0 || g._shareRate == 0) { // Or if shareRate is 0 to prevent division by zero issues with shares.
            currentValueOfLockedShares = 0;
        } else {
            // Note: _shareRate is scaled by 1e8. _totalSharesLocked is the sum of shares.
            // If shareRate was 1e8 initially, shares = tokens.
            // So, value = (shares * currentShareRate) / 1e8 is correct.
            currentValueOfLockedShares = (g._totalSharesLocked * g._shareRate) / 1e8;
        }

        uint256 definedConceptualTotal = circulatingSupply + currentValueOfLockedShares;

        if (definedConceptualTotal == 0) {
            // If the conceptual total supply is zero (no circulating supply and no value in locked shares), the ratio is 0.
            // This results in no share rate increase for the day when processed by _applyDailyRate.
            return 0;
        }
        
        if (currentValueOfLockedShares == 0) {
            // If there's a conceptual total supply but no value is currently locked (e.g., all shares have zero value or no shares exist), the ratio is 0.
            // This also results in no share rate increase for the day via _applyDailyRate.
            return 0;
        }

        // Calculate ratio: (currentValueOfLockedShares * 1e8) / definedConceptualTotal
        uint256 ratio = (currentValueOfLockedShares * 1e8) / definedConceptualTotal;
        
        // Enforce minimum ratio of 0.1% to prevent excessive share rate growth if actual ratio is extremely low.
        // 1e5 in 1e8 scale equals 0.1%.
        if (ratio < 1e5) {
            return 1e5;
        }
        
        return ratio;
    }

    // ------------------------------------------------------------------------
    // Saving Feature (Single 369-day lock)
    // ------------------------------------------------------------------------

    /**
     * @dev Start a new saving by burning the specified number of tokens. 
     *      The user receives "shares" at the current shareRate. 
     *      The saving can only be ended after the fixed 369-day period.
     */
    function saveStart(uint256 newSavedTokens) external {
        require(newSavedTokens > 0, "Oase: Must save more than 0");
        require(newSavedTokens >= 1 * 10**8, "Oase: Minimum save is 1 token");
        require(newSavedTokens < 2**72, "Oase: Amount too large for event packing");
        // use balanceOf from ERC20
        require(balanceOf(msg.sender) >= newSavedTokens, "Oase: insufficient balance");
        require(savingsLists[msg.sender].length < 100, "Oase: User has reached the maximum number of savings");

        // Load globals and take snapshot
        GlobalsCache memory g;
        GlobalsCache memory gSnapshot;
        _globalsLoad(g, gSnapshot);

        // Update daily data, require it to be complete
        bool updated = _dailyDataUpdate(g, _currentDay());
        require(updated, "Oase: Daily data update incomplete, call updateDailyData()");

        // Increment saving ID
        uint40 newSavingId = ++latestSavingId;

        // Calculate shares at the current shareRate FIRST
        // shares = tokens * 1e8 / shareRate
        uint256 newSavingShares = (newSavedTokens * 1e8) / g._shareRate;
        require(newSavingShares > 0, "Oase: Amount too small for current share rate");
        require(newSavingShares < 2**72, "Oase: Shares too large for event packing");

        // Now that we know the shares are valid, burn the tokens
        // Use inherited _burn instead of custom storage
        _burn(msg.sender, newSavedTokens);

        // savingStartDay is next day
        uint256 newSavingStartDay = g._currentDay + 1;
        // savingEndDay is + 369 from start
        uint256 newSavingEndDay = newSavingStartDay + LOCK_LENGTH_DAYS;

        // Create new saving entry
        savingsLists[msg.sender].push(SavingStore({
            savingId: newSavingId,
            savedShares: newSavingShares,
            savedTokens: newSavedTokens,
            savingStartDay: newSavingStartDay,
            savingEndDay: newSavingEndDay
        }));

        // Increase locked supply (original tokens) and total shares locked
        g._lockedTokenSupply += newSavedTokens; // Track initial tokens locked
        g._totalSharesLocked += newSavingShares; // Track total shares

        // Log 
        emit SavingStart(
            uint256(uint40(block.timestamp))
                | (uint256(uint72(newSavedTokens)) << 40)
                | (uint256(uint72(newSavingShares)) << 112),
            msg.sender,
            newSavingId
        );

        // Sync changes back to storage
        _globalsSync(g, gSnapshot);
    }

    /**
     * @dev End a saving that has completed its 369-day lock. 
     *      We compute the final payout by converting shares to tokens at the *current* shareRate.
     *      We also auto-update daily if needed. 
     */
    function saveEnd(uint256 saveIndex, uint40 saveIdParam) external {
        // Load globals and take snapshot
        GlobalsCache memory g;
        GlobalsCache memory gSnapshot;
        _globalsLoad(g, gSnapshot);

        // Update daily data, require it to be complete
        uint256 currentDay = _currentDay();
        bool updated = _dailyDataUpdate(g, currentDay);
        require(updated, "Oase: Daily data update incomplete, call updateDailyData()");

        SavingStore[] storage userSavings = savingsLists[msg.sender];
        require(userSavings.length != 0, "Oase: no savings");
        require(saveIndex < userSavings.length, "Oase: invalid index");

        SavingStore storage stRef = userSavings[saveIndex];
        require(stRef.savingId == saveIdParam, "Oase: savingId mismatch");

        // Ensure 369 days have passed from savingEndDay
        require(g._currentDay >= stRef.savingEndDay, "Oase: saving not matured");

        // Calculate payout
        // payout = shares * shareRate / 1e8
        uint256 payoutTokens = (stRef.savedShares * g._shareRate) / 1e8;

        // Emit the event now, including the number of shares & final payout
        emit SavingEnd(
            uint256(uint40(block.timestamp))
                | (uint256(uint72(stRef.savedShares)) << 40)
                | (uint256(uint72(payoutTokens)) << 112),
            msg.sender,
            stRef.savingId
        );

        // Remove from locked supply
        g._lockedTokenSupply -= stRef.savedTokens;
        g._totalSharesLocked -= stRef.savedShares;

        // Remove saving from array by swapping with last & popping
        _removeSaving(userSavings, saveIndex);

        // Sync
        _globalsSync(g, gSnapshot);

        // Mint final tokens to user 
        _mint(msg.sender, payoutTokens);
    }

    // ------------------------------------------------------------------------
    // Internal Helpers
    // ------------------------------------------------------------------------

    /**
     * @dev Public function to allow anyone to advance the daily data updates.
     *      Processes up to MAX_DAYS_TO_UPDATE days per call.
     *      Call this multiple times if needed to catch up a large backlog.
     */
    function updateDailyData() external {
        GlobalsCache memory g;
        GlobalsCache memory gSnapshot;
        _globalsLoad(g, gSnapshot);

        // Call _dailyDataUpdate but ignore the return value.
        // We just want to process a batch if there's a backlog.
        _dailyDataUpdate(g, _currentDay());

        _globalsSync(g, gSnapshot);
    }

    /**
     * @dev Remove an element from an array by swapping with the last and popping.
     */
    function _removeSaving(SavingStore[] storage arr, uint256 index) internal {
        uint256 lastIndex = arr.length - 1;
        if (index != lastIndex) {
            arr[index] = arr[lastIndex];
        }
        arr.pop();
    }

    /**
     * @dev Returns all of a user's SavingStore entries in a single array.
     */
    function getUserSavings(address user) external view returns (SavingStore[] memory) {
        return savingsLists[user];
    }



    /**
     * @dev Allows users to claim tokens if they are in the merkle tree.
     * @param id Unique identifier for the claim
     * @param amount Amount of tokens to claim
     * @param mintingStartDate Start date of the claim period
     * @param mintingEndDate End date of the claim period
     * @param merkleProof Merkle proof verifying the claim
     * @param signature Optional signature if claiming on behalf of someone else
     */
    function claim(
        uint256 id,
        uint256 amount,
        uint256 mintingStartDate,
        uint256 mintingEndDate,
        bytes32[] calldata merkleProof,
        bytes calldata signature
    ) external {
        require(block.timestamp >= mintingStartDate && block.timestamp <= mintingEndDate, "Not within the minting window.");
        require(!hasClaimedId[id], "This ID has already been claimed.");

        address eligibleAddress;

        if (signature.length > 0) {
            // Case with signature: Recover the eligible address from the signature
            bytes32 messageHash = keccak256(abi.encode(msg.sender, id, amount));
            bytes32 ethSignedMessageHash = MessageHashUtils.toEthSignedMessageHash(messageHash);
            eligibleAddress = ECDSA.recover(ethSignedMessageHash, signature);
        } else {
            eligibleAddress = msg.sender;
        }

        // Construct the node from the eligible address
        bytes32 node = keccak256(abi.encode(id, eligibleAddress, amount, mintingStartDate, mintingEndDate));

        // Verify the provided Merkle proof against the stored Merkle root
        require(MerkleProof.verify(merkleProof, merkleRoot, node), "Invalid Merkle proof.");

        // Record the claim and mint the tokens
        hasClaimedId[id] = true;
        _mint(msg.sender, amount);

        emit Claimed(id, msg.sender, amount, block.timestamp);
    }
}