#lang racket

;;;Dependencias de otros archivos(Rquire)
;;(require "")

;;Funciones que comparto(Provide)
;;(provide)

;;TDA SYSTEM
;;DOM: nombre (string)
;;REC: system 
;;Representacion name (String) X users (String list) X drives (drive list) X current-user (String) X current-drive (char) X current-path (String) x logged(boolean)
(define (system name) 
  (list name '() '() "" #\0 "" 0 '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Constructor :
;;Funcion: make-system
;;Descripcion:
;;Dom:
;;REC:
(define make-system
  (lambda (name users drives current-user current-drive current-path logged elementos)
    (list name users drives current-user current-drive current-path logged elementos)))








;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Pertenencia



;;Funcion:
;;Descripcion:
;;Dom:
;;REC:


;; Capa Pertenencia TDA System
;; member verifica si existe un elemento de una lista, true si existe, else false
(define (exists-system-drive? letter system)
  (member letter (map get-drive-letter (get-system-drives system))))

(define (exists-system-user? user system)
  (member user (get-system-users system)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Selectores
;;Funcion:
;;Descripcion:
;;Dom:
;;REC:

;; Selector TDA System

(define get-system-name car)  ;System -> String
(define get-system-users cadr) ;System -> String List
(define get-system-drives caddr) ;System -> Drive List
(define get-system-current-user cadddr) ;System -> String
(define get-system-current-drive (lambda (system) (car (cdr (cdr (cdr (cdr system))))))) ;System -> char
(define get-system-current-path (lambda (system) (car (cdr (cdr (cdr (cdr (cdr system)))))))) ;System -> String
(define get-system-logged (lambda (system) (car (cdr (cdr (cdr (cdr (cdr(cdr system)))))))));
(define get-system-elementos (lambda (system) (car (cdr (cdr (cdr (cdr (cdr(cdr(cdr system))))))))));


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Modificadores
;;Funcion:
;;Descripcion:
;;Dom:
;;REC:


;; Modificador TDA System

;; agregar nuevo drive (se usa en RF4 add-drive)
(define system-add-drive
  (lambda (system new-drive)
    (make-system (get-system-name system)
                 (get-system-users system)
                 (cons new-drive (get-system-drives system))
                 (get-system-current-user system)
                 (get-system-current-drive system)
                 (get-system-current-path system)
                 (get-system-logged system)
                 (get-system-elementos system))))

;; agregar nuevo user (se usa en RF5 register)
(define system-add-user
  (lambda (system new-user)
    (make-system (get-system-name system)
                 (cons new-user (get-system-users system))
                 (get-system-drives system)
                 (get-system-current-user system)
                 (get-system-current-drive system)
                 (get-system-current-path system)
                 (get-system-logged system)
                 (get-system-elementos system))))

;;hacer Login
(define system-make-login
  (lambda (system new-user)
    (make-system (get-system-name system)
                 (get-system-users system)
                 (get-system-drives system)
                 new-user 
                 (get-system-current-drive system)
                 (get-system-current-path system)
                 1
                 (get-system-elementos system))))

;;hacer Logout
(define system-make-logout
  (lambda (system)
    (make-system (get-system-name system)
                 (get-system-users system)
                 (get-system-drives system)
                 ""
                 (get-system-current-drive system)
                 (get-system-current-path system)
                 0
                 (get-system-elementos system))))



;; RF5. TDA system - register

;; currificado
;; Dom: System X
;;      username (str)
;; Rec: System
(define register
  (lambda (system)
    (lambda (username)
      (if (not (exists-system-user? username system)) ;; si usuario no existe, entonces agregar
          (system-add-user system username) ;; retornar sistema
          system)))) ;; si usuario existe, retornar sistema sin cambios



;; RF5. TDA system - login

;; currificado
;; Dom: System X
;;      username (str)
;; Rec: System

(define login
  (lambda (system)
    (lambda (username)
      (if (exists-system-user? username system)
          (if (= (get-system-logged system) 0)
              (system-make-login system username)
              system)
          system))))



(define logout
  (lambda (system)
    (if (= (get-system-logged system) 1)
        (system-make-logout system)
        system)))




;; RF3. TDA system - run

(define (run system cmd)
  (cmd system))



;; RF4. TDA system - add-drive

;; Dom: System X
;;      letter (char) X name (str) X capacity (int)
;; Rec: System      
(define add-drive
  (lambda (system)
    (lambda (letter name capacity)
      (if (not (exists-system-drive? letter system)) ;; la letra de la unidad es única, no debo agregar una leta que ya exista
          (system-add-drive system    ;;if true then create a new system with the drive
                            (make-drive letter name capacity))
          system)))) ;;else return system








;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Otras funciones
;;Funcion:
;;Descripcion:
;;Dom:
;;REC:




;;;; Implementacion TDAs ;;;;

;TDA Drive
; Capa Constructor Drive
;representación: letter (String) x name (String) x capacity (int) x content (list)
(define make-drive
  (lambda (letter name capacity)
    (list letter name capacity)))


; Capa Selector Drive
(define get-drive-letter car)
(define get-drive-name cadr)
(define get-drive-capacity caddr)


;;;;;;Switch Drive

(define switch-drive
  (lambda (system)
    (lambda (drive)
    (system-make-switch-drive system drive))))

(define system-make-switch-drive
  (lambda (system drive)
           (make-system (get-system-name system)
                 (get-system-users system)       
                 (get-system-drives system)
                 (get-system-current-user system)
                 drive
                 drive
                 (get-system-logged system)
                 (get-system-elementos system))))

;;;;;;;;;;Creacion de carpetas

;;;modificador
(define md
  (lambda (system)
    (lambda (folder )
    (system-make-new-folder system folder))))
;;;Constructor
(define system-make-new-folder
  (lambda (system folder)
           (make-system (get-system-name system)
                 (get-system-users system)       
                 (get-system-drives system)
                 (get-system-current-user system)
                 (get-system-current-drive system)
                 (get-system-current-path system)
                 (get-system-logged system)
                 (cons(make-elemento system folder "f")(get-system-elementos system)))))




;;;Constructor de elemento
;;Dom
;;REC:Lista de usuario(string) x tipo(string) X currentdrive X currentpath X nombreelemento
(define make-elemento
  (lambda (system elemento t)
  (list (get-system-current-user system)
        t
        (get-system-current-drive system)
        (get-system-current-path system)
        elemento)))
 












