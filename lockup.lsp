;====================================================================;
;Drawing Lockup v1.00
;AutoLISP tool for locking AutoCAD drawings so that they can be sent
;and used without the risk of IP theft.
;====================================================================;

;====================================================================;
;Alert Message, Error, Exit
;====================================================================;
(alert
  "\nDO NOT RUN LOCKUP ON AN ORIGINAL DRAWING!
   \nRUN ONLY ON A COPY OF THE ORIGINAL!
   \n
   \nType LOCKUP to lock and UNDOLOCK to unlock."
)

(defun lockerror (msg)
  (if (/= msg "Function cancelled.")
    (princ
      (strcat "\nError: " msg " [" (itoa (getvar "ERRNO")) "]")
    )
    (princ)
  )
  (command "UNDO" "End")
  (Abort "\nLockup was interrupted. Function aborted!")
  (setq *error* olderr)
  (princ)
)

(defun Abort (msg)
  (setvar "filedia" fdia)
  (setvar "cmddia" cdia)
  (setvar "cmdecho" cmd)
  (alert msg)
)

;====================================================================;
;Setup Layers
;====================================================================;
(defun getlayers ()
  (setq lyr (tblnext "layer" t))
  (setq laylist "")
  (while lyr
    (if	(or (and (= (cdr (assoc 62 lyr)) 8)
		 (not (wcmatch (cdr (assoc 2 lyr)) "*|*"))
	    )
	    (and (= (cdr (assoc 62 lyr)) 9)
		 (not (wcmatch (cdr (assoc 2 lyr)) "*|*"))
	    )
	    (and (= (cdr (assoc 62 lyr)) 251)
		 (not (wcmatch (cdr (assoc 2 lyr)) "*|*"))
	    )
	    (and (= (cdr (assoc 62 lyr)) 252)
		 (not (wcmatch (cdr (assoc 2 lyr)) "*|*"))
	    )
	    (and (= (cdr (assoc 62 lyr)) 253)
		 (not (wcmatch (cdr (assoc 2 lyr)) "*|*"))
	    )
	    (and (= (cdr (assoc 62 lyr)) 254)
		 (not (wcmatch (cdr (assoc 2 lyr)) "*|*"))
	    )
	    (and (= (cdr (assoc 62 lyr)) 255)
		 (not (wcmatch (cdr (assoc 2 lyr)) "*|*"))
	    )
	)
      (if (equal laylist "")
	(setq laylist (strcat laylist (cdr (assoc 2 lyr))))
	(setq laylist (strcat laylist "," (cdr (assoc 2 lyr))))
      )
    )
    (setq lyr (tblnext "layer"))
  )
  laylist
)

;====================================================================;
;Setup Back Block
;====================================================================;
(defun backblk (layoutName Mins)
  (if layoutName
    (cond
      ((= layoutName "14MS")
       (setq blist (list '(-4 . "<NOT")
			 '(-4 . "<OR")
			 '(67 . 1)
			 '(0 . "SOLID")
			 '(2 . "SOLID")
			 '(-4 . "OR>")
			 '(-4 . "NOT>")
			 '(-4 . "<OR")
			 (cons 8 (getlayers))
			 '(62 . 8)
			 '(62 . 9)
			 '(62 . 251)
			 '(62 . 252)
			 '(62 . 253)
			 '(62 . 254)
			 '(62 . 255)
			 '(-4 . "OR>")
		   )
       )
      )
      ((= layoutName "14PS")
       (setq blist (list '(67 . 1)
			 '(-4 . "<NOT")
			 '(-4 . "<OR")
			 '(0 . "SOLID")
			 '(2 . "SOLID")
			 '(0 . "VIEWPORT")
			 '(-4 . "OR>")
			 '(-4 . "NOT>")
			 '(-4 . "<OR")
			 (cons 8 (getlayers))
			 '(62 . 8)
			 '(62 . 9)
			 '(62 . 251)
			 '(62 . 252)
			 '(62 . 253)
			 '(62 . 254)
			 '(62 . 255)
			 '(-4 . "OR>")
		   )
       )
      )
      (T
       (setq blist (list (cons 410 layoutName)
			 '(-4 . "<NOT")
			 '(-4 . "<OR")
			 '(0 . "SOLID")
			 '(2 . "SOLID")
			 '(0 . "VIEWPORT")
			 '(-4 . "OR>")
			 '(-4 . "NOT>")
			 '(-4 . "<OR")
			 (cons 8 (getlayers))
			 '(62 . 8)
			 '(62 . 9)
			 '(62 . 251)
			 '(62 . 252)
			 '(62 . 253)
			 '(62 . 254)
			 '(62 . 255)
			 '(-4 . "OR>")
		   )
       )
      )
    )
    (setq blist	(list '(-4 . "<NOT")
		      '(-4 . "<OR")
		      '(0 . "SOLID")
		      '(2 . "SOLID")
		      '(0 . "VIEWPORT")
		      '(-4 . "OR>")
		      '(-4 . "NOT>")
		      '(-4 . "<OR")
		      (cons 8 (getlayers))
		      '(62 . 8)
		      '(62 . 9)
		      '(62 . 251)
		      '(62 . 252)
		      '(62 . 253)
		      '(62 . 254)
		      '(62 . 255)
		      '(-4 . "OR>")
		)
    )
  )
  (setq ssetb (ssget "X" blist))
  (setq viewsset (ssget "X" '((0 . "VIEWPORT"))))
  (if viewsset
    (progn
      (setq n 0)
      (repeat (sslength viewsset)
	(if (setq clipent (assoc 340 (entget (ssname viewsset n))))
	  (ssdel (cdr clipent) ssetb)
	)
	(setq n (1+ n))
      )
    )
  )
  (if ssetb
    (progn
      (setq pt (list 0.0 0.0))
      ;Make block header.
      (entmake
	       (list '(0 . "BLOCK")
		     '(2 . "*anon")
		     '(70 . 1)
		     (cons '10 pt)
	       )
      )
      ;Add entities in selection set to block, repeat for each selection.
      (setq a 0)
      (repeat (sslength ssetb)
	(setq ent2 (entmake (entget (setq ent (ssname ssetb a)))))
	(if (null ent2)
	  (princ (entget (setq ent (ssname ssetb a))))
	)
	;If polyline or block ref with attributes...
	(if (assoc 66 (entget ent))
	  (progn
	    ;Walk down sub-entities until seqend is found.
	    (setq subent (entnext ent))
	    (while (/= (cdr (assoc 0 (entget subent))) "SEQEND")
	      (entmake (entget subent))
	      (setq subent (entnext subent))
	    )

	    ;Now add seqend sub-entity.
	    (setq ent3 (entmake (entget subent)))
	    (if	(null ent3)
	      (princ (entget subent))
	    )
	  )
	)
	;Cleanup by deleting original.
	(entdel ent)
	(setq a (1+ a))
	(c:spin "Making block of background colours...")
      )
      (setq nameb (entmake '((0 . "endblk"))))
      ;Write block end sub-entity.
      (princ "\n  Inserting...\n")

      ;Insert block reference at insertion point.
      ;Note: Check the argument Mins for the method to insert the block.
      ;Note: Mins=T means minsert the block, and Mins=nil means insert it.
      (if Mins
	;Minsert block reference at insertion point.
	(entmake
	  (list	'(0 . "INSERT")
		(CONS '100 "AcDbMInsertBlock")
		(CONS '70 2)
		(CONS '71 2)
		(cons '2 nameb)
		(cons '10 pt)
	  )
	)
	(entmake
	  (list	'(0 . "INSERT")
		(cons '2 nameb)
		(cons '10 pt)
	  )
	)
	;Insert block reference at insertion point.
      )
      (setq bc (entlast))
      (setq bac "back")
      (command "_.draworder" bc "" (strcat "_" bac))
      (setq ssetb nil)
      (setq viewsset nil)
    )
  )
  (princ)
)

;====================================================================;
;Setup Solid Block
;====================================================================;
(defun solidblk	(layoutName Mins)
  (if layoutName
    (cond
      ((= layoutName "14MS")
       (setq slist (list '(-4 . "<NOT")		     '(67 . 1)
			 '(-4 . "NOT>")		     '(-4 . "<OR")
			 '(0 . "SOLID")		     '(2 . "SOLID")
			 '(-4 . "OR>")
			)
       )
      )
      ((= layoutName "14PS")
       (setq slist (list '(67 . 1)
			 '(-4 . "<OR")
			 '(0 . "SOLID")
			 '(2 . "SOLID")
			 '(-4 . "OR>")
		   )
       )
      )
      (T
       (setq slist (list (cons 410 layoutName)
			 '(-4 . "<OR")
			 '(0 . "SOLID")
			 '(2 . "SOLID")
			 '(-4 . "OR>")
		   )
       )
      )
    )
    (setq slist	(list '(-4 . "<OR")
		      '(0 . "SOLID")
		      '(2 . "SOLID")
		      '(-4 . "OR>")
		)
    )
  )
  (setq ssets (ssget "X" slist))
  (if ssets
    (progn
      (setq pt (list 0.0 0.0))
      ;Make block header.
      (entmake
	       (list '(0 . "BLOCK")
		     '(2 . "*anon")
		     '(70 . 1)
		     (cons '10 pt)
	       )
      )
      ;Add entities in selection set to block, repeat for each selection.
      (setq a 0)
      (repeat (sslength ssets)
	(setq ent2 (entmake (entget (setq ent (ssname ssets a)))))
	(if (null ent2)
	  (princ (entget (setq ent (ssname ssets a))))
	)
	;If polyline or block ref with attributes...
	(if (assoc 66 (entget ent))
	  (progn
	    ;Walk down sub-entities until seqend is found.
	    (setq subent (entnext ent))
	    (while (/= (cdr (assoc 0 (entget subent))) "SEQEND")
	      (entmake (entget subent))
	      (setq subent (entnext subent))
	    )

	    ;Now add seqend sub-entity.
	    (setq ent3 (entmake (entget subent)))
	    (if	(null ent3)
	      (princ (entget subent))
	    )
	  )
	)
	;Cleanup by deleting original.
	(entdel ent)
	(setq a (1+ a))
	(c:spin "Making block of solids...")
      )
      (setq names (entmake '((0 . "endblk"))))
      ;Write block end sub-entity.
      (princ "\n  Inserting...\n")

      ;Insert block reference at insertion point.
      ;Note: Check the argument Mins for the method to insert the block.
      ;Note: Mins=T means minsert the block, and Mins=nil means insert it.
      (if Mins
	;Minsert block reference at insertion point.
	(entmake
	  (list	'(0 . "INSERT")
		(CONS '100 "AcDbMInsertBlock")
		(CONS '70 2)
		(CONS '71 2)
		(cons '2 names)
		(cons '10 pt)
	  )
	)
	(entmake
	  (list	'(0 . "INSERT")
		(cons '2 names)
		(cons '10 pt)
	  )
	)
	;Insert block reference at insertion point.
      )
      (setq so (entlast))
      (setq ba "back")
      (command "_.draworder" so "" (strcat "_" ba))
      (setq ssets nil)
    )
  )
  (princ)
)

;====================================================================;
;Setup Anon Block
;====================================================================;
(defun anonBlock (layoutName Mins)
  (if layoutName
    (cond
      ((= layoutName "14MS")
       (setq alist (list '(-4 . "<NOT")
			 '(-4 . "<OR")
			 '(67 . 1)
			 '(0 . "ACAD_PROXY_ENTITY")
			 '(0 . "AEC_*")
			 '(0 . "AECS_*")
			 '(0 . "RTEXT")
			 '(0 . "WIPEOUT")
			 '(0 . "SOLID")
			 '(2 . "SOLID")
			 (cons 8 (getlayers))
			 '(62 . 8)
			 '(62 . 9)
			 '(62 . 251)
			 '(62 . 252)
			 '(62 . 253)
			 '(62 . 254)
			 '(62 . 255)
			 '(-4 . "OR>")
			 '(-4 . "NOT>")
		   )
       )
      )
      ((= layoutName "14PS")
       (setq alist (list '(67 . 1)
			 '(-4 . "<NOT")
			 '(-4 . "<OR")
			 '(0 . "VIEWPORT")
			 '(0 . "ACAD_PROXY_ENTITY")
			 '(0 . "AEC_*")
			 '(0 . "AECS_*")
			 '(0 . "RTEXT")
			 '(0 . "WIPEOUT")
			 '(0 . "SOLID")
			 '(2 . "SOLID")
			 (cons 8 (getlayers))
			 '(62 . 8)
			 '(62 . 9)
			 '(62 . 251)
			 '(62 . 252)
			 '(62 . 253)
			 '(62 . 254)
			 '(62 . 255)
			 '(-4 . "OR>")
			 '(-4 . "NOT>")
		   )
       )
      )
      (T
       (setq alist (list (cons 410 layoutName)
			 '(-4 . "<NOT")
			 '(-4 . "<OR")
			 '(0 . "VIEWPORT")
			 '(0 . "ACAD_PROXY_ENTITY")
			 '(0 . "AECC_*")
			 '(0 . "AEC_*")
			 '(0 . "AECS_*")
			 '(0 . "RTEXT")
			 '(0 . "WIPEOUT")
			 '(0 . "SOLID")
			 '(2 . "SOLID")
			 (cons 8 (getlayers))
			 '(62 . 8)
			 '(62 . 9)
			 '(62 . 251)
			 '(62 . 252)
			 '(62 . 253)
			 '(62 . 254)
			 '(62 . 255)
			 '(-4 . "OR>")
			 '(-4 . "NOT>")
		   )
       )
      )
    )
    (setq alist	(list '(-4 . "<NOT")
		      '(-4 . "<OR")
		      '(0 . "VIEWPORT")
		      '(0 . "ACAD_PROXY_ENTITY")
		      '(0 . "AECC_*")
		      '(0 . "AEC_*")
		      '(0 . "AECS_*")
		      '(0 . "RTEXT")
		      '(0 . "WIPEOUT")
		      '(0 . "SOLID")
		      '(2 . "SOLID")
		      (cons 8 (getlayers))
		      '(62 . 8)
		      '(62 . 9)
		      '(62 . 251)
		      '(62 . 252)
		      '(62 . 253)
		      '(62 . 254)
		      '(62 . 255)
		      '(-4 . "OR>")
		      '(-4 . "NOT>")
		)
    )
  )
  (setq sset (ssget "X" alist))
  (setq viewsset (ssget "X" '((0 . "VIEWPORT"))))
  (if viewsset
    (progn
      (setq n 0)
      (repeat (sslength viewsset)
	(if (setq clipent (assoc 340 (entget (ssname viewsset n))))
	  (ssdel (cdr clipent) sset)
	)
	(setq n (1+ n))
      )
    )
  )
  (if sset
    (progn
      (setq pt (list 0.0 0.0))
      ;Make block header.
      (entmake
	       (list '(0 . "BLOCK")
		     '(2 . "*anon")
		     '(70 . 1)
		     (cons '10 pt)
	       )
      )
      ;Add entities in selection set to block, repeat for each selection.
      (setq a 0)
      (repeat (sslength sset)
	(setq ent2 (entmake (entget (setq ent (ssname sset a)))))
	(if (null ent2)
	  (princ (entget (setq ent (ssname sset a))))
	)
	;If polyline or block ref with attributes...
	(if (assoc 66 (entget ent))
	  (progn
	    ;Walk down sub-entities until seqend is found.
	    (setq subent (entnext ent))
	    (while (/= (cdr (assoc 0 (entget subent))) "SEQEND")
	      (entmake (entget subent))
	      (setq subent (entnext subent))
	    )

	    ;Now add seqend sub-entity.
	    (setq ent3 (entmake (entget subent)))
	    (if	(null ent3)
	      (princ (entget subent))
	    )
	  )
	)
	;Cleanup by deleting original.
	(entdel ent)
	(setq a (1+ a))
	(c:spin "Making block...")
      )
      (setq name (entmake '((0 . "endblk"))))
      ;Write block end sub-entity.
      (princ "\n  Inserting block...\n")

      ;Insert block reference at insertion point.
      ;Note: Check the argument Mins for the method to insert the block.
      ;Note: Mins=T means minsert the block, and Mins=nil means insert it.
      (if Mins
	;Minsert block reference at insertion point.
	(entmake
	  (list	'(0 . "INSERT")
		(CONS '100 "AcDbMInsertBlock")
		(CONS '70 2)
		(CONS '71 2)
		(cons '2 name)
		(cons '10 pt)
	  )
	)
	(entmake
	  (list	'(0 . "INSERT")
		(cons '2 name)
		(cons '10 pt)
	  )
	)
	;Insert block reference at insertion point.
      )
      (setq sset nil)
      (setq viewsset nil)
    )
    (if	layoutName
      (princ (strcat "\nNo entities to lock in " layoutName"."))
    )
  )
  (princ)
)

(defun Finish (vers)
  (setvar "clayer" cla)
  (setvar "tilemode" space)
  (if (= vers 2)
    (command "-layer" "state" "restore" "lockup" "" "")
  )
  (command "-layer" "" "*" "")
  (setvar "proxyshow" 1)
  (command "regen")
  (cond
    ((= cont "Yes")
     (alert
       "\nPaper space only has been locked.
      	\nTo lock model space, run LOCKUP
      	\nagain and do not skip to paper space."
     )
    )
    ((= answer2 "Model")
     (alert "\nAll selected entities have been locked.")
    )
    ((= answer2 nil)
     (alert "\nAll selected entities have been locked.")
    )
  )
  (setq	cont nil
	answer2	nil
  )
  (princ "\nLockup has completed. Your drawing is now locked and protected. ")
  (princ)
)

;====================================================================;
;Lock Paper Space (Fix for R14)
;====================================================================;
;Supply the keyword "14PS" to be recognized by anonBlock
;in order to select all entities that have group code 67 = 1.
(defun goLock14PS ()
  (setvar "tilemode" 0)
  (proxy)
  ;Make anon insert - on paper space
  (anonBlock "14PS" nil)
  ;Make anon insert - on paper space		
  (backblk "14PS" nil)
  ;Make anon insert - on paper space
  (solidBlk "14PS" nil)
  ;Make anon minsert - on paper space
  (anonBlock "14PS" T)
  (command "zoom" "extents")
  (prompt "\n  Paper space has been locked.")
  (Finish 0)
)

;====================================================================;
;Lock Paper Space
;====================================================================;
(defun goLockPS	(vers)
  (if (= vers 0)
    (goLock14PS)
    (progn
      (princ "\nType in layout name to make current: ")
      ;Type in layout to set current.
      (command "layout" "set" pause)
      (while (> (getvar "cmdactive") 0) (command pause))
      (proxy)
      ;Make anon insert - on paper space
      (anonBlock (getvar "CTAB") nil)
      ;Make anon insert - on paper space
      (backblk (getvar "CTAB") nil)
      ;Make anon insert - on paper space
      (solidblk (getvar "CTAB") nil)
      ;Make anon minsert - on paper space
      (anonBlock (getvar "CTAB") T)
      (command "zoom" "extents")
      (initget "Yes No")
      (prompt
	(strcat "\n  Layout " (getvar "ctab") " has been locked.")
      )
      (setq answer
	     (getkword "\nAre there more layouts to lock? Y/<N>: ")
      )
      (cond
	((or (null answer) (= answer "No"))
	 (Finish vers)
	)
	((= answer "Yes")
	 (goLockPS vers)
	)
	(T nil)
      )
    )
  )
)

;====================================================================;
;Main Lock + Sub Functions
;====================================================================;
(defun goLock (vers)
  (setvar "tilemode" 1)
  (if (= vers 2)
    (command "-layer" "state" "save" "lockup" "" "" "")
  )
  (command "-layer" "thaw" "*" "on" "*" "unlock" "*" "")
  (command "zoom" "extents")
  (proxy)
  (if (/= vers 0)
    (progn
      ;Make anon insert in model space.
      (anonBlock "Model" nil)
      ;Make anon insert in model space.		
      (backblk "Model" nil)
      ;Make anon insert in model space.		
      (solidblk "Model" nil)
      ;Make anon minsert in model space.
      (anonBlock "Model" T)
    )
    (progn
      (anonBlock "14MS" nil)
      (backblk "14MS" nil)
      (solidblk "14MS" nil)
      (anonBlock "14MS" T)
    )
  )
  (prompt "\n  Model Space has been locked.")
  (initget "Yes No")
  (setq	answer
	 (getkword "\nDo you want to lock paper space? Y/<N>: ")
  )
  (cond
    ((or (null answer) (= answer "No")) (Finish vers))
    ((= answer "Yes") (goLockPS vers))
    (T nil)
  )
)

(defun states ()
  (if (= vers 2)
    (command "-layer" "state" "save" "lockup" "" "" "")
  )
  (command "-layer" "thaw" "*" "on" "*" "unlock" "*" "")
  (command "graphscr")
  (command "zoom" "extents")
  (goLockps vers)
)

(defun continue	()
  (initget "Yes No")
  (setq	cont (getkword
	       "\nModel Space will not be locked! Continue? Y/<N>: "
	     )
  )
  (cond	((= cont "Yes") (states))
	((= cont "No") (skip))
	((= cont nil) (skip))
  )
)

(defun skip ()
  (initget "Skip Model")
  (setq	answer2
	 (getkword
	   "\nStart in model space or skip to paper space? Skip/<Model>: "
	 )
  )
  (cond	((= answer2 "Skip") (continue))
	((= answer2 "Model") (goLock vers))
	((= answer2 nil) (goLock vers))
  )
)

(defun 14or2k (/ answer)
  (initget "14 2000 2000i")
  (setq	answer
	 (getkword
	   "\nWhat version of AutoCAD are you in? 14/2000<2000i>: "
	 )
  )
  (cond
    ((= answer "14") (setq vers 0))
    ((= answer "2000") (setq vers 1))
    ((= answer "2000i") (setq vers 2))
    ((= answer nil) (setq vers 2))
  )
  (skip)
)

(defun goexp ()
  (progn
    (repeat (sslength sset)
      (command "_explode" (ssname sset CNT))
      (setq CNT (1+ CNT))
      (c:spin "Exploding...")
    )
    (alert (strcat "\n    " (itoa CNT) " Entities exploded."))
  )
  (setq sset nil)
  (princ)
)

(defun xpproxy (/ xpl)
  (alert
    "\n Proxy Entities have been found.
     \n If they are not exploded, they will
  	 \n be omitted from the lockup process."
  )
  (initget "Yes No")
  (setq xpl (getkword "\nExplode proxy entities? Y/<N>: "))
  (if (or (= xpl "No") (= xpl nil))
    (princ)
  )
  (if (= xpl "Yes")
    (goexp)
  )
  (princ)
)

(defun goerase ()
  (progn
    (repeat (sslength wsset)
      (entdel (ssname wsset WCNT))
      (setq WCNT (1+ WCNT))
      (c:spin "Erasing...")
    )
    (alert (strcat "\n    " (itoa WCNT) " Wipeouts erased."))
  )
  (setq wsset nil)
  (princ)
)

(defun goaskerase (/ del)
  (alert
    "\n     Wipeouts have been found."
  )
  (initget "Yes No")
  (setq del (getkword "\nErase wipeouts? Y/<N>: "))
  (if (or (= del "No") (= del nil))
    (princ)
  )
  (if (= del "Yes")
    (goerase)
  )
  (princ)
)

(defun gowipeout (/ where wlist)
  (setq where (getvar "tilemode"))
  (setq cs 67)
  (if (= where 0)
    (setq sp 1)
  )
  (if (= where 1)
    (setq sp 0)
  )
  (setq	wlist (list (cons cs sp)
		    '(0 . "wipeout")
	      )
  )
  (setq WCNT 0)
  (setq wsset (ssget "x" wlist))
  (if (= wsset nil)
    (princ)
  )
  (if (not (= wsset nil))
    (goaskerase)
  )
  (princ)
)

(defun proxy (/ where plist)
  (setq where (getvar "tilemode"))
  (if (= where 0)
    (setq plist	'((-4 . "<NOT")
		  (67 . 0)
		  (-4 . "NOT>")
		  (-4 . "<OR")
		  (0 . "ACAD_PROXY_ENTITY")
		  (0 . "AECC_*")
		  (0 . "AEC_*")
		  (0 . "AECS_*")
		  (0 . "RTEXT")
		  (-4 . "OR>")
		 )
    )
  )
  (if (= where 1)
    (setq plist	'((-4 . "<NOT")
		  (67 . 1)
		  (-4 . "NOT>")
		  (-4 . "<OR")
		  (0 . "ACAD_PROXY_ENTITY")
		  (0 . "AECC_*")
		  (0 . "AEC_*")
		  (0 . "AECS_*")
		  (0 . "RTEXT")
		  (-4 . "OR>")
		 )
    )
  )
  (setq CNT 0)
  (setq sset (ssget "x" plist))
  (if (= sset nil)
    (princ)
  )
  (if (not (= sset nil))
    (xpproxy)
  )
  (gowipeout)
  (princ)
)

;====================================================================;
;Run Undolock
;====================================================================;
(defun c:undolock ()
  ;Undo and reset variables.
  (setvar "cmdecho" 0)
  (princ "\nPlease wait while Lockup is undone.")
  (command "undo" "end")
  (command "undo" "back")
  (setvar "cmdecho" 1)
  (setvar "filedia" 1)
  (setvar "cmddia" 1)
  (setvar "clayer" cla)
  (princ "\nLockup has been undone.")
  (princ)
)

;====================================================================;
;Run Look
;====================================================================;
(defun c:look (/ alist CNT sset)
  (setq	alist '((-4 . "<OR")
		(0 . "ACAD_PROXY_ENTITY")
		(0 . "AECC_*")
		(0 . "AEC_*")
		(0 . "AECS_*")
		(0 . "RTEXT")
		(0 . "WIPEOUT")
		(-4 . "OR>")
	       )
  )
  (setq CNT 0)
  (if alist
    (progn
      (setq sset (ssget "X" alist))
      (if sset
	(repeat	(sslength sset)
	  (setq CNT (1+ CNT))
	)
      )
      (if (= CNT 1)
	(alert (strcat "\n        " (itoa CNT) " entity found."))
      )
      (if (> CNT 1)
	(alert (strcat "\n       " (itoa CNT) " entities found."))
      )
    )
  )
  (if (= sset nil)
    (alert "\nNo entities were found.")
  )
  (princ)
)

;====================================================================;
;Run Spin
;====================================================================;
(defun c:spin (wh)
  (prompt (strcat "\r  "
		  wh
		  (cond	((= sp "|") (setq sp "/"))
			((= sp "/") (setq sp "-"))
			((= sp "-") (setq sp "\\"))
			(T (setq sp "|"))
		  )
	  )
  )
  (princ)
)

;====================================================================;
;Run Lockup Main
;====================================================================;
(defun C:Lockup	(/ start answer)
  (setq	fdia	(getvar "filedia")
	cdia	(getvar "cmddia")
	cmd	(getvar "cmdecho")
	cla	(getvar "clayer")
	space	(getvar "tilemode")
	olderr	*error*
	*error*	lockerror
	cont	nil
	answer2	nil
  )
  (setvar "cmdecho" 0)
  (command "UNDO" "Begin")
  (setvar "filedia" 0)
  (setvar "cmddia" 0)
  (command "undo" "mark")
  (command "-layer" "make" "LOCKUP" "")
  (command "color" "bylayer")
  (setvar "proxyshow" 0)
  (command "regen")
  (initget "Yes No")
  (setq	answer
	 (getkword
	   "\nThis routine will lock the drawing! Do you really want to proceed? Y/<N>: "
	 )
  )
  (cond
    ((or (= answer "No") (null answer))
     (Alert "LOCKUP aborted!")
    )
    ((= answer "Yes") (14or2k))
  )
  (command "UNDO" "End")
  (setq *error* olderr)
  (setvar "filedia" fdia)
  (setvar "cmddia" cdia)
  (setvar "cmdecho" cmd)
  (princ)
)
(princ "\n--> Lockup is loaded.")
(princ "\n--> Type command \"lockup\" to run program.\n")
(princ)