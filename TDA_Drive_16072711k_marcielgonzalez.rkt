#lang racket
;Dependencias 
(require "TDA_System_16072711k_marcielgonzalez.rkt")

;Funciones compartidas
(provide make-drive)
(provide get-drive-letter)
(provide get-drive-name)
(provide get-drive-capacity)
(provide switch-drive)
(provide system-make-switch-drive)


;;;; Implementacion TDAs ;;;;

;TDA Drive
; Capa Constructor Drive
;representación: letter (String) x name (String) x capacity (int) x content (list)
;;DOM:letra unidad X nombre de unidad X capacidad de unidad
;;REC:Drive
(define make-drive
  (lambda (letter name capacity)
    (list letter name capacity)))


; Capa Selector Drive

;;Funcion: get-drive-letter
;;Descripcion: retorna la letra de la unidad
;;Dom:drive
;;REC:letradedrive
(define get-drive-letter car)

;;Funcion: get-drive-name
;;Descripcion: retorna el nombre del drive
;;Dom:drive
;;REC:nombre del drive
(define get-drive-name cadr)

;;Funcion: get-drive-capacity
;;Descripcion: retorna la capacidad de un drive
;;Dom:drive
;;REC:capacidad de un drive
(define get-drive-capacity caddr)


;;;;;;Switch Drive
;;Funcion: switch-drive
;;Descripcion: cambia el current drive del sistema
;;Dom:system X drive
;;REC:System
(define switch-drive
  (lambda (system)
    (lambda (drive)
    (system-make-switch-drive system drive))))


;;Funcion: system-make-switch-drive
;;Descripcion: construye nuevo sistema con un drive
;;Dom:system X drive
;;REC:System
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