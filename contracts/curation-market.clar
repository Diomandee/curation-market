;; Implement the `ft-trait` trait defined in the `ft-trait` contract - SIP 10
;; This can use sugared syntax in real deployment (unit tests do not allow)
(impl-trait .sip010-ft-trait.sip010-ft-trait)

;; ;; Implement the token restriction trait

(define-constant CONTRACT_OWNER tx-sender)
;; Number of blocks to wait for consensus (roughly 48hrs)
(define-constant CYCLE_PERIOD u144)
;; Number of blocks to wait for consensus (roughly 48hrs)
(define-constant CONTRIBUTION u100)
(define-constant QUESTIONS u10)
;; Number of blocks to wait to claim shares after check in (roughly 48hrs)
(define-constant CLAIM_SHARES_PERIOD u322)
;; Default game state used during consensus
;; Error returned for permission denied - stolen from http 403
(define-constant ERR_BAD_REQUEST (err u400))
(define-constant ERR_TIMEOUT_IN_PAST (err u407))
(define-constant ERR_ZERO_VALUE (err u410))
(define-constant ERR_START_TIME_BEFORE (err u414))
(define-constant ERR_PENALTY_GREATER_VALUE (err u400))

(define-constant ERR_CHALLENGE_NOT_FOUND (err u404))
(define-constant PERMISSION_DENIED_ERROR u404)
(define-constant err-owner-only (err u100))
(define-constant err-unknown-token (err u101))
(define-constant err-cannot-be-zero (err u102))
(define-constant err-token-already-exists (err u102))
(define-constant err-insufficient-balance (err u1))
(define-constant err-invalid-sender (err u4))
;; Data variables specific to the deployed token contract
(define-data-var token-name (string-ascii 32) "")
(define-data-var token-symbol (string-ascii 32) "")
(define-data-var token-decimals uint u0)
(define-constant UNSIGNED_ONE_8 (pow u10 u8))
(define-constant MAX_NATURAL_EXPONENT (* u69 UNSIGNED_ONE_8))

(define-data-var costPerToken uint u0)
(define-data-var totalSupply uint u0)
(define-data-var totalEverMinted uint u0)
(define-data-var totalEverWithdrawn uint u0)
(define-data-var poolBalance uint u0)
(define-data-var curation-id uint u0)
(define-data-var LogCostOfTokenUpdate uint u0)
(define-constant ERR_ZERO_VAL (err u100))
(define-constant microStacks u100000)
(define-data-var baseCost uint u1)
(define-data-var totalMinted uint u0)
(define-data-var totalCost uint u0)
(define-data-var totalBonded uint u0)

(define-fungible-token CurationToken)
(define-non-fungible-token challange-nft {token-id: uint, owner: principal})
(define-data-var usersNonce uint u0)

(define-map challangeInfo uint {
cycles: uint,
cycleLength: uint,
challangeDuration: uint,
penalty: (optional uint),
stopTime: uint,
startTime: uint
})

(define-private (get-balance-uint (token-id uint) (who principal))
	(default-to u0 (map-get? token-balances {token-id: token-id, owner: who}))
)

;; Track who deployed the token and whether it has been initialized
(define-data-var deployer-principal principal tx-sender)
(define-data-var is-initialized bool false)

(define-map curation-topic uint {
  id: uint, 
  name: (string-utf8 256)
})

(define-read-only (get-curation-info (id uint))
(let (
  (curate (unwrap-panic (get-curation-topic id)))
  ) (ok {
    id: id,
    topic: (var-get token-name),
    sub-topic: (var-get topic)
  }))
)

(define-read-only (get-balance (owner principal))
  (ok (ft-get-balance CurationToken owner)))

(define-read-only (get-curation-topic (id uint))
(map-get? curation-topic id))

(define-read-only (get-name)
  (ok (var-get token-name)))

(define-read-only (get-symbol)
  (ok (var-get token-symbol)))

(define-read-only (get-decimals)
  (ok (var-get token-decimals)))

(define-read-only (get-total-supply)
  (ok (ft-get-supply CurationToken)))

(define-read-only (get-total-suppl (token-id uint))
	(ok (default-to u0 (map-get? token-supplies token-id)))
)

(define-read-only (get-cost-per-token)
  (ok (var-get costPerToken)))

(define-read-only (total-ever-minted)
  (ok (var-get totalEverMinted)))

(define-read-only (total-ever-withdrawn)
  (ok (var-get totalEverWithdrawn)))

(define-read-only (pool-balance)
  (ok (var-get poolBalance)))

(define-read-only (total-minted)
  (ok (var-get totalMinted)))

(define-read-only (total-supply)
  (ok (var-get totalSupply)))

(define-read-only (total-cost)
  (ok (var-get totalCost)))

(define-read-only (get-log-cost)
  (ok (var-get LogCostOfTokenUpdate)))

(define-public (transfer (amount uint) (sender principal) (recipient principal ) (memo (optional (buff 34) )))
  (begin
    (asserts! (is-eq tx-sender sender) (err u4)) ;; Ensure the originator is the sender principal
    (print (default-to 0x memo))
    (ft-transfer? CurationToken amount sender recipient) ) ) ;; Transfer


	;; Token URI
;; --------------------------------------------------------------------------

;; Variable for URI storage
(define-data-var uri (string-utf8 256) u"")

;; Public getter for the URI
(define-read-only (get-token-uri)
  (ok (some (var-get uri))))

;; Setter for the URI - only the owner can set it
(define-public (set-token-uri (updated-uri (string-utf8 256)))
  (begin
    ;; Print the action for any off chain watchers
    (print { action: "set-token-uri", updated-uri: updated-uri })
           ;; #[allow(unchecked_data)]
    (ok (var-set uri updated-uri))))


;; Token Website
;; --------------------------------------------------------------------------

;; Variable for website storage
(define-data-var website (string-utf8 256) u"")

;; Public getter for the website
(define-read-only (get-token-website)
  (ok (some (var-get website))))

;; Setter for the website - only the owner can set it
(define-public (set-token-website (updated-website (string-utf8 256)))
  (begin
    ;; Print the action for any off chain watchers
    (print { action: "set-token-website", updated-website: updated-website })
           ;; #[allow(unchecked_data)]
    (ok (var-set website updated-website))))


;; Token Website
;; --------------------------------------------------------------------------

;; Variable for topic storage
(define-data-var topic (string-utf8 256) u"")

;; Public getter for the website
(define-read-only (get-token-topic)
  (ok (some (var-get topic))))

;; Setter for the website - only the owner can set it
(define-public (set-token-topic (updated-topic (string-utf8 256)))
  (begin
    ;; Print the action for any off chain watchers
    (print { action: "set-token-topic", updated-website: updated-topic })
           ;; #[allow(unchecked_data)]
    (ok (var-set topic updated-topic))))

;; ------------------------------------------
;; Check in
;; ------------------------------------------

(define-data-var contract-is-enabled bool true)
(define-data-var epoch-length uint block-height)
(define-data-var epoch-number uint u0)
(define-data-var epoch-end-block uint u0)
(define-data-var epoch-start-block uint u0)

;; ------------------------------------------
;; Var & Map Helpers
;; ------------------------------------------

(define-read-only (get-epoch-length )
 (var-get epoch-length))

(define-read-only (get-epoch-number)
  (var-get epoch-number)
)
(define-read-only (get-epoch-end-block)
  (var-get epoch-end-block)
)
(define-read-only (get-epoch-start-block)
  (var-get epoch-start-block)
)
(define-read-only (get-epoch-info (challangeId uint))
(let (
	(challange (unwrap! (map-get? Challanges challangeId) ERR_CHALLENGE_NOT_FOUND))) 
	(ok {
    epoch-length: (var-get epoch-length),
    epoch-number: (var-get epoch-number),
    epoch-end-block: (var-get epoch-end-block),
    epoch-start-block: (var-get epoch-start-block),
  }))
)

;; is-creator function returns true if transaction sender is the creator for given gameId
(define-private (is-creator (challangeId uint))
	(is-eq (some tx-sender) (get creator (get-challange challangeId)))
)

;; is-challenger function returns true if transaction sender is the challenger for given gameId
(define-private (is-challenger (participatorsId uint))
	(is-eq (some tx-sender) (get challenger (get-participator participatorsId)))
)

;; get-game function returns game data for matching gameId
(define-read-only (get-challange (challangeId uint))
  (map-get? Challanges challangeId)
)
(define-read-only (get-challange-info (challangeId uint))
  (map-get? challangeInfo challangeId)
)
;; get-offer function returns offer data for matching offerId
(define-read-only (get-participator (participatorsId uint))
	(map-get? Participators participatorsId)
)


(define-constant DEFAULT_CLAIM {
	creatorClaim: u0, 
	challengerClaim: u0
})

(define-constant DEFAULT_REP { 
	creatorRep: u0,
	challengerRep: u0
	})

;; data maps and vars

;; challengeNonce stores the next gameId
(define-data-var challengeNonce uint u0)
;; offerNonce stores the next offerId

(define-read-only (challenge-nonce)
	(ok (var-get challengeNonce))
)

(define-data-var challengeClaim uint u0)
;; offerNonce stores the next offerId

(define-read-only (claim-nonce)
	(ok (var-get challengeClaim))
)

(define-data-var participatorNonce uint u0)

(define-read-only (participator-nonce)
	(ok (var-get participatorNonce))
)

(define-map submited principal bool)

(define-map Challanges uint {
	creator: principal,	
    challangeName: (string-ascii 32),		;; player address	;; prediction event 
    challangeRules: (string-utf8 256),		;; player address	;; prediction event 
	contributionValue: uint,				;; amount at stake in micro-stx
	timeout: uint,
	startTime: uint,
	isEntity: bool, 
	participatorsAddress: (optional principal),					
	participatorsId: (optional uint),		
	claim: (optional {								
		creatorClaim : uint,		;; stores self declaration of victory by creator
		challengerClaim : uint	;; stores self declaration of victory by challenger		;; stores judged 	;; stores $VERITY amount at the time of accept step
	}),
	rep: (optional {								
		creatorRep : uint,				;; stores $VERITY amount at the time of accept step
		challengerRep : uint,
					;; stores $VERITY amount at the time of accept step
	})
}) 

;; Offers map stores all offers keyed by offerId
(define-map Participators uint {
	challangeId: uint,					;; game  to challenge
	challenger: principal, 			;; challenger address
	participatorsValue: uint,
	totalContributed: uint,				;; amount at stake in micro-stx
}) 


(define-read-only (challange-exist (challangeId uint)) 
    (let (
        (challange (unwrap-panic (get-challange challangeId )))
        (isEntity (get isEntity challange))
    )
    (begin 
    (asserts! (is-some (get-challange challangeId)) ERR_CHALLENGE_NOT_FOUND)
    (asserts! (is-eq true (get isEntity challange )) ERR_CHALLENGE_NOT_FOUND)
    (ok true)
    ))
)


(define-public (start (name (string-ascii 32)) (rules (string-utf8 256)) (ratio uint) (missed (optional uint)) (startTime uint) (days uint))
	(let ((challangeId (var-get challengeNonce))
         (Balance (var-get poolBalance))
         (cycles (/ Balance ratio))
         (cycleLength (* days CYCLE_PERIOD))
         (duration (* cycles cycleLength)) 
         (timeout (+ startTime duration))
    )
		(asserts! (not (is-eq name "")) ERR_BAD_REQUEST)
		(asserts! (not (is-eq rules u"")) ERR_BAD_REQUEST)
		(asserts! (> duration block-height) ERR_TIMEOUT_IN_PAST)
		(asserts! (> days u0) ERR_ZERO_VALUE)
		(asserts! (>= startTime block-height) ERR_START_TIME_BEFORE)
		
		(map-set Challanges challangeId {
            creator: tx-sender,
            challangeName: name, 
            challangeRules: rules, 
            contributionValue: Balance, 
            timeout: timeout,
			startTime: startTime,
			isEntity: true,
		    participatorsId: none,
			participatorsAddress: none, 
            claim: none,
			rep: none
                    })
					
		(map-set challangeInfo challangeId {
            stopTime: timeout,
			startTime: startTime,
            penalty: (some (/ Balance cycles )),
            cycles: cycles,
            cycleLength: cycleLength, 
            challangeDuration: duration,
                    })
		(var-set challengeNonce (+ challangeId u1))
		
	
        (var-set epoch-length block-height)
		(var-set epoch-number block-height)
		(var-set epoch-start-block (+ startTime cycleLength))
		(var-set epoch-end-block timeout)

		(print {action: "start", who: tx-sender, challangeId: challangeId, challange: (get-challange challangeId)})
		(print {action: "start", who: tx-sender, challangeId: challangeId, challange: (get-challange-info challangeId)})
		(ok challangeId)
	)
)



;; Minting 
;; --------------------------------------------------------------------------

(define-private (mint (token-id uint) (amountToMint uint) (ratio uint))
(let (
    (curate (unwrap-panic (get-curation-topic token-id )))
    (challange (get-challange token-id))
    (id (get id curate))
    (contributionValue (get contributionValue challange))
    (Cost (var-get totalCost))
    (Minted (var-get totalMinted))
    (Balance (var-get poolBalance))
    (Supply (var-get totalSupply))
    (pricePerToken (unwrap-panic (get-next-price id amountToMint)))
    (missedPenalty (/ pricePerToken ratio))
  )
  (begin
	(asserts! (and (> amountToMint u0) (>= (- MAX_NATURAL_EXPONENT amountToMint) (ft-get-supply CurationToken))) ERR_ZERO_VAL))
    (var-set totalSupply (+ amountToMint (unwrap-panic (get-total-supply))))
    (var-set costPerToken pricePerToken)
    (var-set poolBalance ( + Balance pricePerToken))
    (var-set costPerToken (var-get LogCostOfTokenUpdate))
    (try! (stx-transfer? pricePerToken tx-sender  (as-contract tx-sender)))
    (try! (ft-mint? CurationToken amountToMint tx-sender))
   (unwrap-panic (start (var-get token-name) (var-get topic) ratio none (+ block-height u100) u3 ))
    (try! (set-balance id (+ (get-balance-or-default id tx-sender) amountToMint) tx-sender))
	(map-set token-supplies id (+ (unwrap-panic (get-total-suppl id)) amountToMint))
    (print {type: "mint", token-id: id, amountToMint: amountToMint})

    (ok pricePerToken)
    )
)

(define-public (updateCon (token-id uint) (amountToMint uint)) 
  (let ( 
    (challange (unwrap! (map-get? Challanges token-id) ERR_CHALLENGE_NOT_FOUND))
    (info (unwrap! (map-get? challangeInfo token-id) ERR_CHALLENGE_NOT_FOUND))
    (curate (unwrap-panic (get-curation-topic token-id)))
    (id (get id curate))
    (oldVal (get contributionValue challange))
    (Balance (var-get poolBalance))

    (pricePerToken (unwrap-panic (get-next-price id amountToMint)))
  ) 
    (begin 
        (try! (ft-mint? CurationToken amountToMint tx-sender))
        (try! (stx-transfer? pricePerToken tx-sender  (as-contract tx-sender)))
        (try! (set-balance id (+ (get-balance-or-default id tx-sender) amountToMint) tx-sender))
        (map-set token-supplies id (+ (unwrap-panic (get-total-suppl id)) amountToMint))
        (var-set poolBalance ( + Balance pricePerToken))
        (map-set Challanges id (merge challange {contributionValue: Balance}))
		    (ok Balance)
    )
  )
)
;; Burning
;; --------------------------------------------------------------------------
(define-public (withdraw (token-id uint) (amountToMint uint))
(let (
    (user tx-sender)
    (curate (unwrap-panic (get-curation-topic token-id)))
    (id (get id curate))
    (Cost (var-get totalCost))
    (Minted (var-get totalMinted))
    (Balance (var-get poolBalance))
    (Supply (var-get totalSupply))
    (pricePerToken (unwrap-panic (get-sell-price id amountToMint)))
    (amount (- Balance pricePerToken))
  )
	(begin
  	(asserts! (and (> amountToMint u0) (>= (- MAX_NATURAL_EXPONENT amountToMint)  (ft-get-supply CurationToken))) ERR_ZERO_VAL))
    (var-set totalSupply (- (unwrap-panic (get-total-supply)) amountToMint))
    (var-set costPerToken pricePerToken)
    (var-set costPerToken (var-get LogCostOfTokenUpdate))
    (var-set poolBalance (- Balance amount))
    (try! (as-contract (stx-transfer? amount tx-sender  user)))
    (try! (ft-burn? CurationToken amountToMint tx-sender))
    (try! (set-balance id (- (get-balance-or-default id tx-sender) amountToMint) tx-sender))
		(map-set token-supplies id (- (unwrap-panic (get-total-suppl id)) amountToMint))
    (print {type: "mint", token-id: id, amountToMint: amountToMint})

    (ok amount)
    )
)



  

;; Initialization
;; --------------------------------------------------------------------------

;; Check to ensure that the same account that deployed the contract is initializing it
;; Only allow this funtion to be called once by checking "is-initialized"
(define-public (initialize (name-to-set (string-ascii 32)) (symbol-to-set (string-ascii 32)) (decimals-to-set uint) (uri-to-set (string-utf8 256)) (website-to-set (string-utf8 256)) (topic-to-set (string-utf8 256)) (initial-owner principal) (initial-amount uint) (initial-rate uint)) 
(let (
	(supply (unwrap-panic (get-total-supply)))
  (curation (var-get curation-id))
	) 
  (begin
    (asserts! (is-eq tx-sender (var-get deployer-principal)) (err PERMISSION_DENIED_ERROR))
    (asserts! (not (var-get is-initialized)) (err PERMISSION_DENIED_ERROR))
    (var-set token-name name-to-set)
    (var-set token-symbol symbol-to-set)
    (var-set token-decimals decimals-to-set)
	  (var-set website website-to-set)
	  (var-set uri uri-to-set)
    (var-set topic topic-to-set)
    (var-set curation-id (+ curation u1))
    (map-set curation-topic curation { id: curation, name: (var-get topic)})
    (unwrap-panic (updateCostOfToken supply))
    (try! (tag-nft-token-id {token-id: curation, owner: tx-sender}))
        (unwrap-panic (mint curation initial-amount initial-rate ))
    (ok true)
    )
  )
)






(define-public (updateCostOfToken (supply uint)) 
  (let (
      (tokenPrice  (/ (* (* supply) u1000000) u4))
      ) 
      (asserts! (>= supply u0) ERR_ZERO_VAL)
      (ok (var-set LogCostOfTokenUpdate tokenPrice))
  )
)

(define-read-only (get-token-price (supply uint))
  (ok (/ (* (* supply) u10) u4))
)


(define-private (get-next-price (token-id uint) (supply uint))
  (let (
  (currentSupply (unwrap-panic (get-total-suppl token-id)))
  (tokenPrice  (/ (* (* (+ supply currentSupply )) u10) u4))
  )
  (var-set LogCostOfTokenUpdate tokenPrice)
  (ok tokenPrice)
  )
)
(define-private (get-sell-price (token-id uint) (supply uint))
  (let (
  (currentSupply (unwrap-panic (get-total-suppl token-id)))
  (tokenPrice  (/ (* (* (- currentSupply supply)) u10) u4))
  )
  (var-set LogCostOfTokenUpdate tokenPrice)
  (ok tokenPrice)
  )
)





(define-map token-balances {token-id: uint, owner: principal} uint)
(define-map bond-balances {token-id: (string-utf8 256), owner: principal} uint)
(define-map token-supplies uint uint)
(define-map token-owned principal (list 200 uint))
(define-map token-bonds principal (list 200 (string-utf8 256)))



(define-public (transfer- (token-id uint) (amount uint) (sender principal) (recipient principal))
	(let
		(
			(sender-balance (get-balance-uint token-id sender))
		)
		(asserts! (or (is-eq sender tx-sender) (is-eq sender contract-caller)) err-invalid-sender)
		(asserts! (<= amount sender-balance) err-insufficient-balance)
		(try! (ft-transfer? CurationToken amount sender recipient))
		(try! (tag-nft-token-id {token-id: token-id, owner: sender}))
		(try! (tag-nft-token-id {token-id: token-id, owner: recipient}))
				(try! (set-balance token-id (- sender-balance amount) sender))
				(try! (set-balance token-id (+ (get-balance-uint token-id recipient) amount) recipient))
		(print {type: "sft_transfer_event", token-id: token-id, amount: amount, sender: sender, recipient: recipient})
		(ok true)
	)
)

(define-public (transfer-memo (token-id uint) (amount uint) (sender principal) (recipient principal) (memo (buff 34)))
	(begin
		(try! (transfer- token-id amount sender recipient))
		(print memo)
		(ok true)
	)
)

(define-private (transfer-many-iter (item {token-id: uint, amount: uint, sender: principal, recipient: principal}) (previous-response (response bool uint)))
	(match previous-response prev-ok (transfer- (get token-id item) (get amount item) (get sender item) (get recipient item)) prev-err previous-response)
)

(define-public (transfer-many (transfers (list 100 {token-id: uint, amount: uint, sender: principal, recipient: principal})))
	(fold transfer-many-iter transfers (ok true))
)

(define-private (transfer-many-memo-iter (item {token-id: uint, amount: uint, sender: principal, recipient: principal, memo: (buff 34)}) (previous-response (response bool uint)))
	(match previous-response prev-ok (transfer-memo (get token-id item) (get amount item) (get sender item) (get recipient item) (get memo item)) prev-err previous-response)
)

(define-public (transfer-many-memo (transfers (list 100 {token-id: uint, amount: uint, sender: principal, recipient: principal, memo: (buff 34)})))
	(fold transfer-many-memo-iter transfers (ok true))
)


(define-read-only (get-token-balances (token-id uint) (owner principal)) 
(map-get? token-balances {token-id: token-id, owner: owner} ))

(define-private (tag-nft-token-id (nft-token-id {token-id: uint, owner: principal}))
	(begin
		(and
			(is-some (nft-get-owner? challange-nft nft-token-id))
			(try! (nft-burn? challange-nft nft-token-id (get owner nft-token-id)))
		)
		(nft-mint? challange-nft nft-token-id (get owner nft-token-id))
	)
)

(define-read-only (get-bond-balances (token-id (string-utf8 256)) (owner principal)) 
(map-get? bond-balances {token-id: token-id, owner: owner} ))

(define-read-only (get-token-owned (owner principal))
    (default-to (list) (map-get? token-owned owner))
)
(define-read-only (get-bonds-owned (owner principal))
    (default-to (list) (map-get? token-bonds owner))
)
(define-constant ERR-TOO-MANY-POOLS (err u2004))



(define-private (set-balance (token-id uint) (balance uint) (owner principal))
    (begin
		(and 
			(is-none (index-of (get-token-owned owner) token-id))
			(map-set token-owned owner (unwrap! (as-max-len? (append (get-token-owned owner) token-id) u200) ERR-TOO-MANY-POOLS))
		)	
	    (map-set token-balances {token-id: token-id, owner: owner} balance)
        (ok true)
    )
)
(define-private (set-bond (token-id uint) (bonds (string-utf8 256)) (balance uint) (owner principal))
    (begin
		(and 
			(is-none (index-of (get-bonds-owned owner) bonds))
			(map-set token-bonds owner (unwrap! (as-max-len? (append (get-bonds-owned owner) bonds) u200) ERR-TOO-MANY-POOLS))
		)	
	    (map-set bond-balances {token-id: bonds, owner: owner} balance)
        (ok true)
    )
)
(define-private (get-balance-or-default (token-id uint) (who principal))
	(default-to u0 (map-get? token-balances {token-id: token-id, owner: who}))
)




(begin (initialize "Stack" "STD" u10 u"none" u"none" u"Hypes" tx-sender u16 u10))