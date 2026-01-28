;; ============================================================
;; Sudoku Solver - Backtracking vs DFS with MRV Heuristic
;; Tracking set-cell attempts for fair comparison
;; ============================================================

;; ===== 1. Test Puzzles =====

(defun create-easy-puzzle ()
  '((5 3 0 0 7 0 0 0 0)
    (6 0 0 1 9 5 0 0 0)
    (0 9 8 0 0 0 0 6 0)
    (8 0 0 0 6 0 0 0 3)
    (4 0 0 8 0 3 0 0 1)
    (7 0 0 0 2 0 0 0 6)
    (0 6 0 0 0 0 2 8 0)
    (0 0 0 4 1 9 0 0 5)
    (0 0 0 0 8 0 0 7 9)))

(defun create-medium-puzzle ()
  '((5 3 0 0 7 0 0 0 0)
    (6 0 0 1 9 5 0 0 0)
    (0 9 8 0 0 0 0 6 0)
    (8 0 0 0 6 0 0 0 0)
    (4 0 0 8 0 3 0 0 1)
    (7 0 0 0 2 0 0 0 6)
    (0 6 0 0 0 0 2 8 0)
    (0 0 0 4 1 9 0 0 5)
    (0 0 0 0 8 0 0 7 9)))

(defun create-hard-puzzle ()
  '((5 3 0 0 7 0 0 0 0)
    (6 0 0 1 9 5 0 0 0)
    (0 9 8 0 0 0 0 6 0)
    (8 0 0 0 6 0 0 0 0)
    (4 0 0 8 0 3 0 0 0)
    (7 0 0 0 2 0 0 0 6)
    (0 6 0 0 0 0 2 8 0)
    (0 0 0 4 1 9 0 0 5)
    (0 0 0 0 8 0 0 7 9)))

;; ===== 2. Basic Board Functions =====

(defun copy-board (board)
  "Make a copy of the board"
  (mapcar #'copy-list board))

(defun get-cell (board row col)
  "Get the number at position (row, col)"
  (nth col (nth row board)))

(defun set-cell (board row col value)
  "Put a number at position (row, col)"
  (setf (nth col (nth row board)) value)
  board)

;; ===== 3. Validation Functions =====
;; These make sure we put CORRECT numbers only!

(defun is-valid-row (board row num)
  "Check: Is num already in this row?"
  (not (member num (nth row board))))

(defun is-valid-col (board col num)
  "Check: Is num already in this column?"
  (not (member num (mapcar (lambda (r) (nth col r)) board))))

(defun is-valid-box (board row col num)
  "Check: Is num already in the 3x3 box?"
  (let* ((box-row (* (floor row 3) 3))
         (box-col (* (floor col 3) 3))
         (box-nums nil))
    (dotimes (i 3)
      (dotimes (j 3)
        (push (get-cell board (+ box-row i) (+ box-col j)) box-nums)))
    (not (member num box-nums))))

(defun is-valid-move (board row col num)
  "MAIN VALIDATION: Can we put num at (row, col)?
   Checks all 3 Sudoku rules: row, column, and 3x3 box"
  (and (is-valid-row board row num)
       (is-valid-col board col num)
       (is-valid-box board row col num)))

;; ===== 4. Find Empty Cells =====

(defun find-empty-cell (board)
  "Find first empty cell (value = 0), scan left-to-right, top-to-bottom"
  (dotimes (row 9)
    (dotimes (col 9)
      (when (zerop (get-cell board row col))
        (return-from find-empty-cell (list row col)))))
  nil)

(defun is-solved (board)
  "Check if puzzle is complete (no empty cells)"
  (null (find-empty-cell board)))

;; ===== 5. MRV Heuristic Functions =====
;; MRV = Minimum Remaining Values
;; h(cell) = how many valid numbers can go in this cell

(defun count-valid-numbers (board row col)
  "Count how many numbers (1-9) are valid for this empty cell
   This is the heuristic value h(cell)
   Lower h(cell) = more constrained = solve first"
  (let ((count 0))
    (dotimes (num 9)
      (when (is-valid-move board row col (+ num 1))
        (incf count)))
    count))

(defun find-best-cell (board)
  "Find empty cell with MINIMUM h(cell) - MRV heuristic
   This cell has fewest options, so we solve it first"
  (let ((best-cell nil)
        (min-count 10))
    (dotimes (row 9)
      (dotimes (col 9)
        (when (zerop (get-cell board row col))
          (let ((count (count-valid-numbers board row col)))
            (when (< count min-count)
              (setf min-count count)
              (setf best-cell (list row col)))))))
    best-cell))

;; ===== 6. Method 1: Basic Backtracking =====

(defvar *backtrack-nodes* 0)
(defvar *backtrack-attempts* 0)

(defun solve-backtrack (board &optional (count-stats nil))
  "Method 1: Basic Backtracking
   - Choose cell: First empty cell (row-by-row scan)
   - Try numbers: 1, 2, 3, 4, 5, 6, 7, 8, 9 in order"
  (when count-stats 
    (setf *backtrack-nodes* 0)
    (setf *backtrack-attempts* 0))
  (labels ((solve (b)
             (when count-stats (incf *backtrack-nodes*))
             (when (is-solved b)
               (return-from solve t))
             (let ((empty (find-empty-cell b)))
               (when empty
                 (let ((row (first empty))
                       (col (second empty)))
                   ;; Try numbers 1-9 in order
                   (dotimes (num 9)
                     (let ((digit (+ num 1)))
                       (when (is-valid-move b row col digit)
                         (when count-stats (incf *backtrack-attempts*))
                         (set-cell b row col digit)
                         (when (solve b)
                           (return-from solve t))
                         (set-cell b row col 0))))))
               nil)))
    (let ((copy (copy-board board)))
      (if (solve copy)
          (values copy *backtrack-nodes* *backtrack-attempts*)
          (values nil *backtrack-nodes* *backtrack-attempts*)))))

;; ===== 7. Method 2: DFS with MRV Heuristic =====

(defvar *dfs-nodes* 0)
(defvar *dfs-attempts* 0)

(defun solve-dfs-mrv (board &optional (count-stats nil))
  "Method 2: DFS with MRV Heuristic
   - Choose cell: Cell with minimum h(cell) using MRV
   - Try numbers: 1, 2, 3, 4, 5, 6, 7, 8, 9 in order (same as Method 1)"
  (when count-stats 
    (setf *dfs-nodes* 0)
    (setf *dfs-attempts* 0))
  (labels ((solve (b)
             (when count-stats (incf *dfs-nodes*))
             (when (is-solved b)
               (return-from solve t))
             (let ((empty (find-best-cell b)))
               (when empty
                 (let ((row (first empty))
                       (col (second empty)))
                   ;; Try numbers 1-9 in order (same as backtracking)
                   (dotimes (num 9)
                     (let ((digit (+ num 1)))
                       (when (is-valid-move b row col digit)
                         (when count-stats (incf *dfs-attempts*))
                         (set-cell b row col digit)
                         (when (solve b)
                           (return-from solve t))
                         (set-cell b row col 0))))))
               nil)))
    (let ((copy (copy-board board)))
      (if (solve copy)
          (values copy *dfs-nodes* *dfs-attempts*)
          (values nil *dfs-nodes* *dfs-attempts*)))))

;; ===== 8. Display Functions =====

(defun print-board (board)
  "Display the Sudoku board with grid lines"
  (dotimes (row 9)
    (when (and (> row 0) (zerop (mod row 3)))
      (format t "------+-------+------~%"))
    (dotimes (col 9)
      (when (and (> col 0) (zerop (mod col 3)))
        (format t "| "))
      (let ((val (get-cell board row col)))
        (format t "~a " (if (zerop val) "." val))))
    (format t "~%")))

(defun count-empty (board)
  "Count how many empty cells in the puzzle"
  (let ((cnt 0))
    (dotimes (row 9)
      (dotimes (col 9)
        (when (zerop (get-cell board row col))
          (incf cnt))))
    cnt))

(defun verify-solution (board)
  "Verify the solution is correct by checking all constraints"
  (let ((valid t))
    ;; Check all rows contain 1-9
    (dotimes (row 9)
      (unless (equal (sort (copy-list (nth row board)) #'<)
                     '(1 2 3 4 5 6 7 8 9))
        (setf valid nil)))
    ;; Check all columns contain 1-9
    (dotimes (col 9)
      (let ((col-vals (mapcar (lambda (r) (nth col r)) board)))
        (unless (equal (sort (copy-list col-vals) #'<)
                       '(1 2 3 4 5 6 7 8 9))
          (setf valid nil))))
    ;; Check all 3x3 boxes contain 1-9
    (dotimes (br 3)
      (dotimes (bc 3)
        (let ((box nil))
          (dotimes (i 3)
            (dotimes (j 3)
              (push (get-cell board (+ (* br 3) i) (+ (* bc 3) j)) box)))
          (unless (equal (sort (copy-list box) #'<)
                         '(1 2 3 4 5 6 7 8 9))
            (setf valid nil)))))
    valid))

;; ===== 9. Test Functions =====

(defun test-one-puzzle (level-name puzzle)
  "Test both methods on one puzzle and display results"
  (format t "~%========================================~%")
  (format t "  ~a Puzzle~%" level-name)
  (format t "========================================~%")
  (format t "~%Initial Board:~%")
  (print-board puzzle)
  (let ((empty-count (count-empty puzzle)))
    (format t "~%Empty cells: ~d~%" empty-count)
    
    (let (bt-nodes bt-attempts bt-time dfs-nodes dfs-attempts dfs-time)
      ;; Test Method 1: Backtracking
      (format t "~%--- Method 1: Backtracking ---~%")
      (let* ((start (get-internal-run-time))
             (results (multiple-value-list (solve-backtrack puzzle t)))
             (solution (first results))
             (nodes (second results))
             (attempts (third results))
             (end (get-internal-run-time))
             (time-ms (* 1000 (/ (- end start) (float internal-time-units-per-second)))))
        (format t "Nodes expanded: ~d~%" nodes)
        (format t "Set-cell attempts: ~d~%" attempts)
        (format t "Time: ~,2f ms~%" time-ms)
        (format t "Verification: ~a~%" (if (verify-solution solution) "Correct" "Incorrect"))
        (setf bt-nodes nodes bt-attempts attempts bt-time time-ms))
      
      ;; Test Method 2: DFS with MRV
      (format t "~%--- Method 2: DFS with MRV Heuristic ---~%")
      (let* ((start (get-internal-run-time))
             (results (multiple-value-list (solve-dfs-mrv puzzle t)))
             (solution (first results))
             (nodes (second results))
             (attempts (third results))
             (end (get-internal-run-time))
             (time-ms (* 1000 (/ (- end start) (float internal-time-units-per-second)))))
        (format t "Nodes expanded: ~d~%" nodes)
        (format t "Set-cell attempts: ~d~%" attempts)
        (format t "Time: ~,2f ms~%" time-ms)
        (format t "Verification: ~a~%" (if (verify-solution solution) "Correct" "Incorrect"))
        (format t "~%Solution:~%")
        (print-board solution)
        (setf dfs-nodes nodes dfs-attempts attempts dfs-time time-ms))
      
      (list level-name empty-count bt-nodes bt-attempts dfs-nodes dfs-attempts bt-time dfs-time))))

(defun test-all ()
  "Test all three difficulty levels and show summary"
  (format t "~%========================================~%")
  (format t "  Sudoku Solver - Complete Test~%")
  (format t "========================================~%")
  
  (let ((results (list
                  (test-one-puzzle "Easy" (create-easy-puzzle))
                  (test-one-puzzle "Medium" (create-medium-puzzle))
                  (test-one-puzzle "Hard" (create-hard-puzzle)))))
    
    (format t "~%==========================================================================~%")
    (format t "  Performance Summary~%")
    (format t "==========================================================================~%")
    (format t "~%Difficulty | Empty | BT Nodes | BT Attempts | MRV Nodes | MRV Attempts~%")
    (format t "-----------|-------|----------|-------------|-----------|--------------~%")
    (dolist (data results)
      (format t "~10a | ~5d | ~8d | ~11d | ~9d | ~12d~%"
              (first data)    ; level name
              (second data)   ; empty cells
              (third data)    ; BT nodes
              (fourth data)   ; BT set-cell attempts
              (fifth data)    ; MRV nodes
              (sixth data)))    ; MRV set-cell attempts

    (format t "~%Difficulty | BT Time | MRV Time~%")
    (format t "-----------|---------|----------~%")
    (dolist (data results) 
            (format t "~10a | ~7,2f | ~8,2f~%"
              (first data)    ; level name
              (seventh data)  ; BT time
              (eighth data))))) ; MRV time

;; ===== 10. Interactive Menu =====

(defun show-menu ()
  (format t "~%========================================~%")
  (format t "  Sudoku Solver - Main Menu~%")
  (format t "========================================~%")
  (format t "1 - Solve Easy Puzzle~%")
  (format t "2 - Solve Medium Puzzle~%")
  (format t "3 - Solve Hard Puzzle~%")
  (format t "4 - Test All Puzzles~%")
  (format t "0 - Exit~%")
  (format t "========================================~%")
  (format t "Enter choice (0-4): "))

(defun get-choice ()
  (let ((input (read)))
    (if (and (integerp input) (<= 0 input 4))
        input
        (progn
          (format t "Invalid input! Please enter 0-4.~%")
          (get-choice)))))

(defun main ()
  (loop
    (show-menu)
    (let ((choice (get-choice)))
      (case choice
        (1 (test-one-puzzle "Easy" (create-easy-puzzle)))
        (2 (test-one-puzzle "Medium" (create-medium-puzzle)))
        (3 (test-one-puzzle "Hard" (create-hard-puzzle)))
        (4 (test-all))
        (0 (progn
             (format t "~%Thank you for using Sudoku Solver!~%")
             (format t "Goodbye!~%~%")
             (return)))
        (otherwise
         (format t "Invalid choice!~%"))))))

;; ===== Start Program =====

(format t "~%")
(format t "========================================~%")
(format t "  Welcome to Sudoku Solver~%")
(format t "  Backtracking vs MRV Heuristic~%")
(format t "========================================~%")

(main)
```