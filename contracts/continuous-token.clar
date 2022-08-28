;; Implement the `ft-trait` trait defined in the `ft-trait` contract - SIP 10
;; This can use sugared syntax in real deployment (unit tests do not allow)
(impl-trait .sip010-ft-trait.sip010-ft-trait)

;; ;; Implement the token restriction trait

;; Error returned for permission denied - stolen from http 403
(define-constant PERMISSION_DENIED_ERROR u404)
(define-constant ERR_BAD_REQUEST (err u400))

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
(define-data-var LogCostOfTokenUpdate uint u0)
(define-constant ERR_ZERO_VAL (err u100))
(define-constant microStacks u100000)
(define-data-var baseCost uint u1)
(define-data-var totalMinted uint u0)
(define-data-var totalCost uint u0)

;; Track who deployed the token and whether it has been initialized
(define-data-var deployer-principal principal tx-sender)
(define-data-var is-initialized bool false)
(define-map token-balances {token-id: uint, owner: principal} uint)
(define-map token-supplies uint uint)
(define-map token-owned principal (list 200 uint))


;; Meta Read Only Functions for reading details about the contract - conforms to SIP 10
;; --------------------------------------------------------------------------

;; Defines built in support functions for tokens used in this contract
;; A second optional parameter can be added here to set an upper limit on max total-supply
(define-fungible-token CurationToken)

;; Get the token balance of the specified owner in base units
(define-read-only (get-balance (owner principal))
  (ok (ft-get-balance CurationToken owner)))

;; Returns the token name
(define-read-only (get-name)
  (ok (var-get token-name)))

;; Returns the symbol or "ticker" for this token
(define-read-only (get-symbol)
  (ok (var-get token-symbol)))

;; Returns the number of decimals used
(define-read-only (get-decimals)
  (ok (var-get token-decimals)))

;; Returns the total number of tokens that currently exist
(define-read-only (get-total-supply)
  (ok (ft-get-supply CurationToken)))

(define-read-only (get-total-suppl (token-id uint))
	(ok (default-to u0 (map-get? token-supplies token-id)))
)


;; Write function to transfer tokens between accounts - conforms to SIP 10
;; --------------------------------------------------------------------------

;; Transfers tokens to a recipient
;; The originator of the transaction (tx-sender) must be the 'sender' principal
;; Smart contracts can move tokens from their own address by calling transfer with the 'as-contract' modifier to override the tx-sender.

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
(define-data-var website (string-ascii 32) "")

;; Public getter for the website
(define-read-only (get-token-website)
  (ok (some (var-get website))))

;; Setter for the website - only the owner can set it
(define-public (set-token-website (updated-website (string-ascii 32)))
  (begin
    ;; Print the action for any off chain watchers
    (print { action: "set-token-website", updated-website: updated-website })
           ;; #[allow(unchecked_data)]
    (ok (var-set website updated-website))))


;; Token Website
;; --------------------------------------------------------------------------

;; Variable for topic storage
(define-data-var topic (string-ascii 32) "")

;; Public getter for the website
(define-read-only (get-token-topic)
  (ok (some (var-get topic))))

;; Setter for the website - only the owner can set it
(define-public (set-token-topic (updated-topic (string-ascii 32)))
  (begin
    ;; Print the action for any off chain watchers
    (print { action: "set-token-topic", updated-website: updated-topic })
           ;; #[allow(unchecked_data)]
    (ok (var-set topic updated-topic))))

;; Getters
;; --------------------------------------------------------------------------

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


;; Minting 
;; --------------------------------------------------------------------------

(define-public (mint (token-id uint) (amountToMint uint))
(let (
    (Cost (var-get totalCost))
    (Minted (var-get totalMinted))
    (Balance (var-get poolBalance))
    (Supply (var-get totalSupply))
    (pricePerToken (unwrap-panic (get-next-price token-id amountToMint)))
  )
  (begin
	(asserts! (and (> amountToMint u0) (>= (- MAX_NATURAL_EXPONENT amountToMint) (ft-get-supply CurationToken))) ERR_ZERO_VAL))
    (var-set totalSupply (+ amountToMint (unwrap-panic (get-total-supply))))
    (var-set costPerToken pricePerToken)
    (var-set poolBalance ( + Balance pricePerToken))
    (var-set costPerToken (var-get LogCostOfTokenUpdate))
    (try! (stx-transfer? pricePerToken tx-sender  (as-contract tx-sender)))
   
    (try! (ft-mint? CurationToken amountToMint tx-sender))
    
    (try! (set-balance token-id (+ (get-balance-or-default token-id tx-sender) amountToMint) tx-sender))
	(map-set token-supplies token-id (+ (unwrap-panic (get-total-suppl token-id)) amountToMint))
    (print {type: "mint", token-id: token-id, amountToMint: amountToMint})

    (ok pricePerToken)
    )
)

;; Burning
;; --------------------------------------------------------------------------
(define-public (withdraw (token-id uint) (amountToMint uint))
(let (
    (user tx-sender)
    (Cost (var-get totalCost))
    (Minted (var-get totalMinted))
    (Balance (var-get poolBalance))
    (Supply (var-get totalSupply))
    (pricePerToken (unwrap-panic (get-sell-price token-id amountToMint)))
    (amount (- Balance pricePerToken))
  )
	(begin
	(asserts! (and (> amountToMint u0) (>= (- MAX_NATURAL_EXPONENT amountToMint) (ft-get-supply CurationToken))) ERR_ZERO_VAL))
    (var-set totalSupply (- (unwrap-panic (get-total-supply)) amountToMint))
    (var-set costPerToken pricePerToken)
    (var-set costPerToken (var-get LogCostOfTokenUpdate))
    (var-set poolBalance (- Balance amount))

    (try! (as-contract (stx-transfer? amount tx-sender  user)))
  
    (try! (ft-burn? CurationToken amountToMint tx-sender))
    
    (try! (set-balance token-id (- (get-balance-or-default token-id tx-sender) amountToMint) tx-sender))
		(map-set token-supplies token-id (- (unwrap-panic (get-total-suppl token-id)) amountToMint))
    (print {type: "mint", token-id: token-id, amountToMint: amountToMint})

    (ok amount)
    )
)


;; Initialization
;; --------------------------------------------------------------------------

;; Check to ensure that the same account that deployed the contract is initializing it
;; Only allow this funtion to be called once by checking "is-initialized"
(define-public (initialize (token-id uint) (name-to-set (string-ascii 32)) (symbol-to-set (string-ascii 32)) (decimals-to-set uint) (uri-to-set (string-utf8 256)) (website-to-set (string-ascii 32)) (topic-to-set (string-ascii 32)) (initial-owner principal) (initial-amount uint))
(let (
	(supply (unwrap-panic (get-total-supply)))
	) 
  (begin
    (asserts! (is-eq tx-sender (var-get deployer-principal)) (err PERMISSION_DENIED_ERROR))
    (asserts! (not (var-get is-initialized)) (err PERMISSION_DENIED_ERROR))
    (var-set is-initialized true) ;; Set to true so that this can't be called again
    (var-set token-name name-to-set)
    (var-set token-symbol symbol-to-set)
    (var-set token-decimals decimals-to-set)
	   (var-set website website-to-set)
	  (var-set uri uri-to-set)
    (var-set topic topic-to-set)
    
    (unwrap-panic (get-next-price token-id supply))
    (unwrap-panic (mint token-id initial-amount))

    (ok true))))




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






(define-read-only (get-token-balances (token-id uint) (owner principal)) 
(map-get? token-balances {token-id: token-id, owner: owner} ))

(define-read-only (get-token-owned (owner principal))
    (default-to (list) (map-get? token-owned owner))
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
(define-private (get-balance-or-default (token-id uint) (who principal))
	(default-to u0 (map-get? token-balances {token-id: token-id, owner: who}))
)




