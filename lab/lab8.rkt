#lang htdp/isl+


(require htdp/dir)
 
; A File is a (make-file String Integer Date String)
;(define-struct file [name size date content])
; - where name is the name of the file (including extension)
; - size is the size of the file in bytes
; - date is the last modified date of the file
; - and content is the contents of the file
 
; A Date is a (make-date Integer Integer Integer Integer Integer Integer)
;(define-struct date [year month day hours minutes seconds])
; - where year is the year the file was modified
; - month is the month the file was modified
; - day is the day the file was modified
; - hours is the hour the file was modified
; - minutes is the minute the file was modified
; - and seconds is the second the file was modified
 
; A Directory is a (make-dir Symbol [List-of Directory] [List-of File])
;(define-struct dir [name dirs files])
; - where name is the directory path
; - dirs is the list of sub-directories in this directory
; - and files is the list of files in this directory
;   (not including the ones in sub-directories)
 
 
(define EMPTY-DIR (make-dir 'hopes&dreams '() '()))
(define CAREER
  (make-dir
   'Career
   (list (make-dir 'CareerApplications
                   '()
                   (list (make-file "CoverLetter.doc" 31744
                                    (make-date 2015 9 20 11 36 25) "")
                         (make-file "EmploymentApplication.pdf" 231010
                                    (make-date 2015 10 13 13 10 0) "")))
         (make-dir 'CareerMyJob
                   '()
                   (list (make-file "BackgroundCheck.pdf" 1040138
                                    (make-date 2016 8 23 10 27 10) "")
                         (make-file "I9.pdf" 963654
                                    (make-date 2015 11 20 15 49 45) "")
                         (make-file "JobOffer.pdf" 507887
                                    (make-date 2015 11 20 15 49 0) ""))))
   (list (make-file "References.docx" 11634
                    (make-date 2016 8 6 9 55 15) "")
         (make-file "Resume.doc" 34816
                    (make-date 2016 10 12 13 18 12) "")
         (make-file "Transcript.doc" 140288
                    (make-date 2015 9 11 9 3 0) ""))))
 
; file-temp : File -> ?
(define (file-temp f)
  (... (file-name f) (file-size f) (date-temp (file-date f)) (file-content f)))
 
; date-temp : Date -> ?
(define (date-temp d)
  (... (date-year d) (date-month d) (date-hours d) (date-minutes d) (date-seconds d)))
 
; directory-temp : Directory -> ?
(define (directory-temp d)
  (... (dir-name d) (lod-temp (dir-dirs d)) (lof-temp (dir-files d))))
 
; lod-temp : [List-of Directory] -> ?
(define (lod-temp lod)
  (cond [(empty? lod) ...]
        [(cons? lod) (... (directory-temp (first lod)) (lod-temp (rest lod)))]))
 
; lof-temp : [List-of File] -> ?
(define (lof-temp lof)
  (cond [(empty? lof) ...]
        [(cons? lof) (... (file-temp (first lof)) (lof-temp (rest lof)))]))


; count-files : Directory -> Natural
; Count the number of files in d and its sub-directories recursively
(define (count-files d)
  (foldr (λ (sub-dir count)
           (+ (count-files sub-dir) count))
         (length (dir-files d))
         (dir-dirs d)))
(check-expect (count-files EMPTY-DIR) 0)
(check-expect (count-files CAREER) 8)

; 1 ===============
(create-dir ".")

; 2 ============

; Directory String -> Boolean
(define (file-exists-in-dir d s)
  (or
   (file-exists-in-files (dir-files d) s)
   (file-exists-in-dirs (dir-dirs d) s)
   ))

; List-of Directory
(define (file-exists-in-dirs lod s)
  (cond [(empty? lod) #f]
        [(cons? lod) (or (file-exists-in-dir (first lod) s)
                         (file-exists-in-dirs (rest lod) s))]))

; List-of File
(define (file-exists-in-files lof s)
  (cond [(empty? lof) #f]
        [(cons? lof) (or (file-same-name (first lof) s)
                         (file-exists-in-files (rest lof) s))]))

(define (file-same-name f s)
  (string=? s (file-name f)))

(check-expect (file-exists-in-dir CAREER "EmploymentApplication.pdf") #t)
(check-expect (file-exists-in-dir CAREER "Resume.doc") #t)

; 3 ===========================================================

; Directory -> Number
(define (dir-size d)
  (+ (dirs-size (dir-dirs d)) (files-size (dir-files d))))

(define (dirs-size lod)
  (cond [(empty? lod) 0]
        [(cons? lod) (+ (dir-size (first lod))
                        (dirs-size (rest lod)))]))

(define (files-size lof)
  (cond [(empty? lof) 0]
        [(cons? lof) (+ (file-size (first lof))
                        (files-size (rest lof)))]))

(check-expect (dir-size EMPTY-DIR) 0)
(check-expect (dir-size CAREER) 2961171)

; 4 =========================================
; Constructs a directory from given directory with all
; files with a given name rename as a given rename name
; rename-files : Directory String String -> Directory
(define (dir-rename d src target)
  (make-dir (dir-name d) (dirs-rename (dir-dirs d)) (files-rename (dir-files d))))
 
; dirs-rename : [List-of Directory] -> 
(define (dirs-rename lod src target)
  (cond [(empty? lod) '()]
        [(cons? lod) (cons (dir-rename (first lod))
                           (dirs-rename (rest lod)))]))
 
; files-rename : [List-of File] -> ?
(define (files-rename lof src target)
  (cond [(empty? lof) '()]
        [(cons? lof) (cons (file-rename (first lof))
                           (files-rename (rest lof)))]))

; File string string -> File
(define (file-rename f src target)
  (if (string=? src (file-name f))
      (make-file target (file-size f) (file-date f) (file-content f))
      f))

; 5 ===================================================================




