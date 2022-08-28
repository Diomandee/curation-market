

;; (impl-trait .sip013-semi-fungible-token-trait.sip013-semi-fungible-token-trait)

(use-trait ft-trait .sip010-ft-trait.sip010-ft-trait)
(define-constant contract-owner tx-sender)

(define-fungible-token challange-nft)
(define-non-fungible-token semi-fungible-token-id {token-id: uint, owner: principal})
(define-map token-balances {token-id: uint, owner: principal} uint)
(define-map token-supplies uint uint)
(define-data-var usersNonce uint u0)

(define-constant CONTRACT_OWNER tx-sender)
;; Number of blocks to wait for consensus (roughly 48hrs)
(define-constant CYCLE_PERIOD u144)
;; Number of blocks to wait for consensus (roughly 48hrs)
(define-constant CONTRIBUTION u100)
(define-constant QUESTIONS u10)
;; Number of blocks to wait to claim shares after check in (roughly 48hrs)
(define-constant CLAIM_SHARES_PERIOD u322)
;; Default game state used during consensus

;; error codes
;;
(define-constant ERR_BAD_REQUEST (err u400))
(define-constant ERR_UNAUTHORIZED (err u401))
(define-constant ERR_CHECKED_IN (err u402))
(define-constant ERR_FORBIDDEN (err u403))
(define-constant ERR_CHALLENGE_NOT_FOUND (err u404))
(define-constant ERR_OFFER_NOT_FOUND (err u405))
(define-constant ERR_OFFER_FOR_GAME_NOT_FOUND (err u406))
(define-constant ERR_TIMEOUT_IN_PAST (err u407))
(define-constant ERR_TIMEOUT_NOT_REACHED (err u408))
(define-constant ERR_CONSENSUS_PERIOD_TIMEDOUT (err u409))
(define-constant ERR_ZERO_VALUE (err u410))
(define-constant ERR_CONTRIBUTION_NOT_STARTED (err u411))
(define-constant ERR_SHOULD_NEVER_HAPPEN (err u419))
(define-constant ERR_PENALTY_GREATER_VALUE (err u400))
(define-constant ERR_NOT_EQUAL (err u401))
(define-constant ERR_NOT_ORDER (err u420))
(define-constant ERR_START_TIME_BEFORE (err u414))
(define-constant err-owner-only (err u100))
(define-constant err-unknown-token (err u101))
(define-constant err-cannot-be-zero (err u102))
(define-constant err-token-already-exists (err u102))
(define-constant err-insufficient-balance (err u1))
(define-constant err-invalid-sender (err u4))
(define-data-var challange-symbol (string-utf8 2048) u"")


;; returns number of registered users, used for activation and tracking user IDs
(define-read-only (get-registered-users-nonce)
  (var-get usersNonce)
)

;; store user principal by user id
(define-map Users
  uint
  principal
)

;; store user id by user principal
(define-map UserIds
  principal
  uint
)

;; returns (some userId) or none
(define-read-only (get-user-id (user principal))
  (map-get? UserIds user)
)

;; returns (some userPrincipal) or none
(define-read-only (get-user (userId uint))
  (map-get? Users userId)
)

;; returns user ID if it has been created, or creates and returns new ID
(define-private (get-or-create-user-id (user principal))
  (match
    (map-get? UserIds user)
    value value
    (let
      (
        (newId (+ u1 (var-get usersNonce)))
      )
      (map-set Users newId user)
      (map-set UserIds user newId)
      (var-set usersNonce newId)
      newId
    )
  )
)





;; Returns the symbol or "ticker" for this token
(define-read-only (get-symbol)
  (ok (var-get challange-symbol)))

(define-private (set-balance (token-id uint) (balance uint) (owner principal))
	(map-set token-balances {token-id: token-id, owner: owner} balance)
)

(define-private (get-balance-uint (token-id uint) (who principal))
	(default-to u0 (map-get? token-balances {token-id: token-id, owner: who}))
)

(define-read-only (get-balance (token-id uint) (who principal))
	(ok (get-balance-uint token-id who))
)

(define-read-only (get-overall-balance (who principal))
	(ok (ft-get-balance challange-nft who))
)

(define-read-only (get-total-supply (token-id uint))
	(ok (default-to u0 (map-get? token-supplies token-id)))
)

(define-read-only (get-overall-supply)
	(ok (ft-get-supply challange-nft))
)

(define-read-only (get-decimals (token-id uint))
	(ok u0)
)


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


;; Challange Name
;; --------------------------------------------------------------------------

;; Variable for website storage
(define-data-var challange-name (string-ascii 32) "")

;; Public getter for the website
(define-read-only (get-challange-name)
  (ok (some (var-get challange-name))))

;; Setter for the website - only the owner can set it
(define-public (set-challange-name (updated-challange-name (string-ascii 32)))
  (begin
    ;; Print the action for any off chain watchers
    (print { action: "set-challange-name", updated-website: updated-challange-name })
           ;; #[allow(unchecked_data)]
    (ok (var-set challange-name updated-challange-name))))

(define-read-only (get-name)
  (ok (var-get challange-name)))


;; Challange Rules
;; --------------------------------------------------------------------------

;; Variable for website storage
(define-data-var challange-rules (string-ascii 32) "")

;; Public getter for the website
(define-read-only (get-challange-rules)
  (ok (some (var-get challange-rules))))

;; Setter for the website - only the owner can set it
(define-public (set-challange-rules (updated-challange-rules (string-ascii 32)))
  (begin
    ;; Print the action for any off chain watchers
    (print { action: "set-challange-rules", updated-website: updated-challange-rules })
           ;; #[allow(unchecked_data)]
    (ok (var-set challange-rules updated-challange-rules))))



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
(define-map challangeInfo uint {
	cycles: uint,
    cycleLength: uint,
    checkInFreq: uint,
	challangeDuration: uint,
    penalty: uint,
	stopTime: uint,
	startTime: uint
} 
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


(define-public (start (name (string-ascii 32)) (rules (string-utf8 256)) (value uint) (missedPenalty uint) (startTime uint) (days uint))
	(let ((challangeId (var-get challengeNonce))
         (cycles (/ value missedPenalty))
         (cycleLength (* days CYCLE_PERIOD))
         (duration (* cycles cycleLength)) 
         (timeout (+ startTime duration))
    )
		(asserts! (not (is-eq name "")) ERR_BAD_REQUEST)
		(asserts! (not (is-eq rules u"")) ERR_BAD_REQUEST)
		(asserts! (> duration block-height) ERR_TIMEOUT_IN_PAST)
		(asserts! (> value u0) ERR_ZERO_VALUE)
		(asserts! (> days u0) ERR_ZERO_VALUE)
		(asserts! (>= startTime block-height) ERR_START_TIME_BEFORE)
		(asserts! (> value missedPenalty) ERR_PENALTY_GREATER_VALUE)
		
		(map-set Challanges challangeId {
            creator: tx-sender,
            challangeName: name, 
            challangeRules: rules, 
            contributionValue: value, 
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
            penalty: missedPenalty,
            cycles: cycles,
            checkInFreq: cycles,
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

(define-public (updateValue (challangeId uint) (newVal uint))
(let (
	(challange (unwrap! (map-get? Challanges challangeId) ERR_CHALLENGE_NOT_FOUND))
	(oldVal (get contributionValue challange))
	) 
	(begin 
		(map-set Challanges challangeId (merge challange {
		contributionValue: (+ oldVal newVal) }))
		(ok (+ oldVal newVal))
	)
)
)


;; stop function removes game from Challanges map data
(define-public (stop (challangeId uint))
	(let (
		(challange (unwrap! (map-get? Challanges challangeId) ERR_CHALLENGE_NOT_FOUND)))
		(asserts! (is-eq tx-sender (get creator challange)) ERR_FORBIDDEN)
		(asserts! (is-none (get participatorsId challange)) ERR_FORBIDDEN)
		(try! (as-contract (stx-transfer? (get contributionValue challange) tx-sender (get creator challange))))
		(map-delete Challanges challangeId)

		(print {action: "stop", who: tx-sender, challangeId: challangeId, challange: challange})
		(ok challangeId)
	)
)




;; challenge function creates new offer for a game
(define-public (challenger (challangeId uint) (matchValue uint))
	(let ((participatorsId (var-get participatorNonce))
		(challenge (unwrap! (map-get? Challanges challangeId) ERR_CHALLENGE_NOT_FOUND))
		(contributionValue (get contributionValue challenge))
		(startTime (get startTime challenge))
		
		)
		(asserts! (< block-height (- startTime CONTRIBUTION)) ERR_CONTRIBUTION_NOT_STARTED)
		(asserts! (is-eq matchValue (get contributionValue challenge)) ERR_NOT_EQUAL)
		(asserts! (not (is-eq tx-sender (get creator challenge))) ERR_FORBIDDEN)
		(asserts! (is-none (get participatorsId challenge)) ERR_FORBIDDEN)
        (asserts! (is-none (map-get? submited tx-sender)) ERR_FORBIDDEN)

		(map-set submited tx-sender true)
		(map-set Participators participatorsId {
            challangeId: challangeId, 
            challenger: tx-sender, 
            participatorsValue: matchValue,
			totalContributed: (+ matchValue contributionValue), 
        })
		(var-set participatorNonce (+ participatorsId u1))

		(print {action: "challenge", who: tx-sender, participator: (get-participator participatorsId), challange: (get-challange challangeId)})
		(ok participatorsId)
	)
)
 
;; withdraw function allows challenger to cancel & refund previous offers which are not yet accepted/locked
(define-public (withdraw (participatorsId uint))
	(let ((participator (unwrap! (map-get? Participators participatorsId) ERR_OFFER_NOT_FOUND))
			(challangeId (get challangeId participator))
			(challenge (unwrap! (map-get? Challanges challangeId) ERR_CHALLENGE_NOT_FOUND))
			(lockedValue (get participatorsId challenge))
			(participatorsValue (get participatorsValue participator))
			(opposition (get challenger participator))
		)
		(asserts! (not (and (not (is-none lockedValue)) (is-eq participatorsId (default-to u0 (get participatorsId challenge))))) ERR_FORBIDDEN)
		(asserts! (is-eq tx-sender opposition) ERR_FORBIDDEN)
		(try! (as-contract (stx-transfer? participatorsValue tx-sender opposition)))
		(map-delete Participators participatorsId)

		(print {action: "withdraw", who: tx-sender, participator: participator, opposition: opposition})
		(ok true)
	)
)
;; accept function locks the game & offer together 
;; #[allow(unchecked_data)]
(define-public (accept (challangeId uint))
	(let (
		(challenge (unwrap! (map-get? Challanges challangeId) ERR_CHALLENGE_NOT_FOUND))
		(contributionValue (get contributionValue challenge))

		(creator (get creator challenge))
		(rep (default-to DEFAULT_REP (get rep challenge)))

		(creatorRep (unwrap! (contract-call? .challange-me-reputation mint creator contributionValue) ERR_SHOULD_NEVER_HAPPEN)))

		(asserts! (is-none (get participatorsId challenge)) ERR_FORBIDDEN)
		(asserts! (is-eq tx-sender (get creator challenge)) ERR_FORBIDDEN)
        (not (is-eq tx-sender (get creator challenge))) 
		(asserts! (is-none (map-get? checkin tx-sender)) ERR_FORBIDDEN)

	    (map-insert checkin tx-sender true)

		(map-set Challanges challangeId 
		(merge challenge {
			rep: (some (merge rep {	
				creatorRep: creatorRep		
			}))}))

		(print {action: "claim", who: tx-sender, challenge: challenge})
		(ok challangeId)
	)
)

;; accept function locks the game & offer together 
;; #[allow(unchecked_data)]

(define-map checkin principal bool)



(define-map checkedIn1 principal bool)
(define-map checkedIn2 principal bool)
(define-map checkedIn3 principal bool)
(define-map checkedIn4 principal bool)
(define-map checkedIn5 principal bool)


(define-public (question1 (challangeId uint) (amount uint))
  (let (
    (creator tx-sender)
	(start-block (var-get epoch-start-block))
	(challenge (unwrap! (map-get? Challanges challangeId) ERR_CHALLENGE_NOT_FOUND))
	(challange_Info (unwrap! (map-get? challangeInfo challangeId) ERR_CHALLENGE_NOT_FOUND))
	(minus100 (- start-block u100))
	(plus100 (+ start-block u100))
	(update (update-epoch challangeId))
  )
	(asserts! (> block-height minus100) ERR_TIMEOUT_NOT_REACHED)
	(asserts! (< block-height plus100) ERR_TIMEOUT_IN_PAST)
  	(asserts! (is-eq amount (get penalty challange_Info)) ERR_NOT_EQUAL)
	(asserts! (is-eq tx-sender creator) ERR_FORBIDDEN)
	
	(asserts! (is-none (map-get? checkedIn1 tx-sender)) ERR_CHECKED_IN)
	(map-insert checkedIn1 tx-sender true)

    (try! (contract-call? .challange-me-reputation transfer amount creator .challange-me-freedom none))


    (print {action: "claim", who: tx-sender, challenge: challenge})
    (ok update)

  )
)

(define-public (question2 (challangeId uint) (amount uint))
  (let (
    (creator tx-sender)
	(start-block (var-get epoch-start-block))
	(challenge (unwrap! (map-get? Challanges challangeId) ERR_CHALLENGE_NOT_FOUND))
	(challange_Info (unwrap! (map-get? challangeInfo challangeId) ERR_CHALLENGE_NOT_FOUND))
	(minus100 (- start-block u100))
	(plus100 (+ start-block u100))
	(update (update-epoch challangeId))
  )
	(asserts! (> block-height minus100) ERR_TIMEOUT_NOT_REACHED)
	(asserts! (< block-height plus100) ERR_TIMEOUT_IN_PAST)
  	(asserts! (is-eq amount (get penalty challange_Info)) ERR_NOT_EQUAL)
	(asserts! (is-eq tx-sender creator) ERR_FORBIDDEN)

	(asserts! (is-some (map-get? checkedIn1 tx-sender)) ERR_NOT_ORDER)
	(asserts! (is-none (map-get? checkedIn2 tx-sender)) ERR_CHECKED_IN)

	(map-insert checkedIn2 tx-sender true)

    (try! (contract-call? .challange-me-reputation transfer amount creator (as-contract tx-sender) none))
    (print {action: "claim", who: tx-sender, challenge: challenge})
    (ok update)

  )
)
(define-public (question3 (challangeId uint) (amount uint))
  (let (
    (creator tx-sender)
	(start-block (var-get epoch-start-block))
	(challenge (unwrap! (map-get? Challanges challangeId) ERR_CHALLENGE_NOT_FOUND))
	(challange_Info (unwrap! (map-get? challangeInfo challangeId) ERR_CHALLENGE_NOT_FOUND))
	(minus100 (- start-block u100))
	(plus100 (+ start-block u100))
	(update (update-epoch challangeId))
  )
	(asserts! (> block-height minus100) ERR_TIMEOUT_NOT_REACHED)
	(asserts! (< block-height plus100) ERR_TIMEOUT_IN_PAST)
  	(asserts! (is-eq amount (get penalty challange_Info)) ERR_NOT_EQUAL)
	(asserts! (is-eq tx-sender creator) ERR_FORBIDDEN)

	(asserts! (is-some (map-get? checkedIn1 tx-sender)) ERR_NOT_ORDER)
	(asserts! (is-some (map-get? checkedIn2 tx-sender)) ERR_CHECKED_IN)
	(asserts! (is-none (map-get? checkedIn3 tx-sender)) ERR_NOT_ORDER)
	(asserts! (is-none (map-get? checkedIn4 tx-sender)) ERR_NOT_ORDER)
	(asserts! (is-none (map-get? checkedIn5 tx-sender)) ERR_NOT_ORDER)
		(map-insert checkedIn3 tx-sender true)

    (try! (contract-call? .challange-me-reputation transfer amount creator (as-contract tx-sender) none))
    (print {action: "claim", wo: tx-sender, challenge: challenge})
    (ok update)

  )
)
(define-public (question4 (challangeId uint) (amount uint))
  (let (
    (creator tx-sender)
	(start-block (var-get epoch-start-block))
	(challenge (unwrap! (map-get? Challanges challangeId) ERR_CHALLENGE_NOT_FOUND))
	(challange_Info (unwrap! (map-get? challangeInfo challangeId) ERR_CHALLENGE_NOT_FOUND))
	(minus100 (- start-block u100))
	(plus100 (+ start-block u100))
	(update (update-epoch challangeId))
  )
	(asserts! (> block-height minus100) ERR_TIMEOUT_NOT_REACHED)
	(asserts! (< block-height plus100) ERR_TIMEOUT_IN_PAST)
  	(asserts! (is-eq amount (get penalty challange_Info)) ERR_NOT_EQUAL)
	(asserts! (is-eq tx-sender creator) ERR_FORBIDDEN)
    (asserts! (is-some (map-get? checkedIn1 tx-sender)) ERR_NOT_ORDER)
	(asserts! (is-some (map-get? checkedIn2 tx-sender)) ERR_NOT_ORDER)
	(asserts! (is-some (map-get? checkedIn3 tx-sender)) ERR_NOT_ORDER)
	(asserts! (is-none (map-get? checkedIn4 tx-sender)) ERR_CHECKED_IN)
	(asserts! (is-none (map-get? checkedIn5 tx-sender)) ERR_NOT_ORDER)
		(map-insert checkedIn4 tx-sender true)

    (try! (contract-call? .challange-me-reputation transfer amount creator (as-contract tx-sender) none))
    (print {action: "claim", who: tx-sender, challenge: challenge})
    (ok update)

  )
)
(define-public (question5 (challangeId uint) (amount uint))
  (let (
    (creator tx-sender)
	(start-block (var-get epoch-start-block))
	(challenge (unwrap! (map-get? Challanges challangeId) ERR_CHALLENGE_NOT_FOUND))
	(challange_Info (unwrap! (map-get? challangeInfo challangeId) ERR_CHALLENGE_NOT_FOUND))
	(minus100 (- start-block u100))
	(plus100 (+ start-block u100))
	(update (update-epoch challangeId))
  )
	(asserts! (> block-height minus100) ERR_TIMEOUT_NOT_REACHED)
	(asserts! (< block-height plus100) ERR_TIMEOUT_IN_PAST)
  	(asserts! (is-eq amount (get penalty challange_Info)) ERR_NOT_EQUAL)
	(asserts! (is-eq tx-sender creator) ERR_FORBIDDEN)
	(asserts! (is-some (map-get? checkedIn1 tx-sender)) ERR_NOT_ORDER)
	(asserts! (is-some (map-get? checkedIn2 tx-sender)) ERR_NOT_ORDER)
	(asserts! (is-some (map-get? checkedIn3 tx-sender)) ERR_NOT_ORDER)
	(asserts! (is-some (map-get? checkedIn4 tx-sender)) ERR_NOT_ORDER)
	(asserts! (is-none (map-get? checkedIn5 tx-sender)) ERR_CHECKED_IN)
	(map-insert checkedIn5 tx-sender true)

    (try! (contract-call? .challange-me-reputation transfer amount creator (as-contract tx-sender) none))
    (print {action: "claim", who: tx-sender, challenge: challenge})
    (ok update)

  )
)



(define-private (penalty-missed (challangeId uint)) 
(let (
     (challange_Info (unwrap! (map-get? challangeInfo challangeId) ERR_CHALLENGE_NOT_FOUND))
	 (penalty (get penalty challange_Info))
	)
(ok true)
	 )
)

(define-private (update-epoch (challangeId uint)) 
(let (
    (creator tx-sender)
	(challange_Info (unwrap! (map-get? challangeInfo challangeId) ERR_CHALLENGE_NOT_FOUND))
	(length (var-get epoch-length))
	(number (var-get epoch-number))
	(cycleLength (get cycleLength challange_Info))
	(start-block (var-get epoch-start-block))
	)
   (begin 
	    (var-set epoch-length (+ length cycleLength)) 
   		(var-set epoch-number (+ number u1)) 
		(var-set epoch-start-block (+ (var-get epoch-start-block) cycleLength))

	)
(ok true)
))

(define-private (get-current-block-time) 
  (default-to u0 (get-block-info? time block-height)))




;; (contract-call? 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM.wrapped-stx wrap u10)
;; (contract-call? 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM.wrapped-stx transfer u10 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM 'ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5 none)


;; (begin 
;; (start u"jjhhh" u"smd" u10 u2 u200 u3)
;; )
;; (begin 
;; (accept u0 )
;; )
