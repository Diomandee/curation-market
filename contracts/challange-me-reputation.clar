(impl-trait .sip010-ft-trait.sip010-ft-trait)

;; SIP010 trait on mainnet
;; (impl-trait 'SP3FBR2AGK5H9QBDH3EEN6DF8EK8JY7RX8QJ5SVTE.sip-010-trait-ft-standard.sip-010-trait)

(define-constant CONTRACT_OWNER tx-sender)

;; honesty is limitless!
(define-fungible-token reputation-coin)

(define-constant ERR_OWNER_ONLY (err u100))
(define-constant ERR_NOT_TOKEN_OWNER (err u101))

;; #[allow(unchecked_data)]
(define-public (transfer (amount uint) (sender principal) (recipient principal) (memo (optional (buff 34))))
	(begin
		(asserts! (or (is-eq tx-sender sender) (is-eq contract-caller sender)) ERR_OWNER_ONLY)
		(ft-transfer? reputation-coin amount sender recipient)
	)
)

(define-read-only (get-name)
	(ok "Reputation")
)

(define-read-only (get-symbol)
	(ok "REP")
)

(define-read-only (get-decimals)
	(ok u6) ;; same as stx
)

(define-read-only (get-balance (who principal))
	(ok (ft-get-balance reputation-coin who))
)

(define-read-only (get-total-supply)
	(ok (ft-get-supply reputation-coin))
)

(define-read-only (get-token-uri)
	(ok none)
)

;; #[allow(unchecked_data)]
(define-public (mint (account principal) (amount uint))
	(begin
		;; (asserts! (is-eq contract-caller .challange-me) ERR_OWNER_ONLY)
        (unwrap-panic (ft-mint? reputation-coin amount account))
		;; (ft-mint? reputation-coin amount recipient)
      (ok amount)
    )
)



;;#[allow(unchecked_data)]
(define-public (burn (amount uint) (sender principal))
	(begin
		(asserts! (> amount u0) (err u1))
		(asserts! (is-eq tx-sender sender) ERR_OWNER_ONLY)
		(ft-burn? reputation-coin amount sender)
	)
)


;; ---------------------------------------------------------
;; Wrap / Unwrap
;; ---------------------------------------------------------

(define-public (wrap (amount uint))
  (begin
    (asserts! (> amount u0 ) ERR_OWNER_ONLY)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (ft-mint? reputation-coin amount tx-sender)
  )
)

(define-public (unwrap (amount uint))
  (let (
    (recipient tx-sender)
  )
  (asserts! (> amount u0 ) ERR_OWNER_ONLY)
    (try! (as-contract (stx-transfer? amount (as-contract tx-sender) recipient)))
    (ft-burn? reputation-coin amount recipient)
  )
)