#lang racket
;(require "TDA_Drive_16072711k_marcielgonzalez.rkt")

;(provide make-system)
(provide system)
(provide exists-system-drive?)
(provide exists-system-user?)
(provide get-system-name)
(provide get-system-users)
(provide get-system-drives)
(provide get-system-current-user)
(provide get-system-current-drive)
(provide get-system-current-path)
(provide get-system-logged)
(provide get-system-elementos)
(provide system-add-drive)
(provide system-add-user)
(provide system-make-login)
(provide system-make-logout)
(provide logout)
(provide login)
(provide register)
(provide run)
(provide add-drive)
(provide system-make-new-folder)
(provide make-elemento)
(provide get-system-elements)
(provide filtro-xuser)
(provide filtro-xpath)
(provide cd)
(provide dospuntos)
(provide slash)
(provide cdfolder)
(provide system-make-path)
(provide estamos-en-raiz)
(provide es-xuser?)
;(provide md)


;;;Dependencias de otros archivos(Rquire)
;;(require "")

;;Funciones que comparto(Provide)
;;(provide)

;;TDA SYSTEM
;;DOM: nombre (string)
;;REC: system 
;;Representacion name (String) X users (String list) X drives (drive list) X current-user (String) X current-drive (char) X current-path (String) x logged(boolean) x Elementos
(define (system name) 
  (list name '() '() "N" #\C "C" 0 '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Constructor :
;;Funcion: make-system
;;Descripcion:
;;Dom:name users drives current-user current-drive current-path logged elementos
;;REC:system
(define make-system
  (lambda (name users drives current-user current-drive current-path logged elementos)
    (list name users drives current-user current-drive current-path logged elementos)))








;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Pertenencia



;; Capa Pertenencia TDA System
;; member verifica si existe un elemento de una lista, true si existe, else false
;;Dom:letrade unidad X system
;;REC:System
(define (exists-system-drive? letter system)
  (member letter (map get-drive-letter (get-system-drives system))))


;; member verifica si existe un elemento de una lista, true si existe, else false
;;Dom:letrade usuario X system
;;REC:System
(define (exists-system-user? user system)
  (member user (get-system-users system)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Selectores


;; Selector TDA System
;;Funcion:get-system-name
;;Descripcion: Retorna el nombre del sistema
;;Dom: system
;;REC: nombre del system
(define get-system-name car)  ;System -> String


;;Funcion:get-system-users
;;Descripcion: Retorna los usuarios registrados en el sistema
;;Dom: system
;;REC: usuarios registrados del sistema
(define get-system-users cadr) ;System -> String List


;;Funcion:get-system-drive
;;Descripcion: Retorna las unidades creadas en el sistema
;;Dom: system
;;REC: unidades creadas en el sistema
(define get-system-drives caddr) ;System -> Drive List


;;Funcion:get-system-current-user
;;Descripcion: Retorna el usuario logueado en el sistema
;;Dom: system
;;REC: usuario logueado en el sistema
(define get-system-current-user cadddr) ;System -> String

;;Funcion:get-system-current-drive
;;Descripcion: Retorna la unidad actual del sistema
;;Dom: system
;;REC:unidad actual del sistema
(define get-system-current-drive (lambda (system) (car (cdr (cdr (cdr (cdr system))))))) ;System -> char

;;Funcion:get-system-current-path
;;Descripcion: Retorna la ruta actual del sistema
;;Dom: system
;;REC:rutal actua del sistema
(define get-system-current-path (lambda (system) (car (cdr (cdr (cdr (cdr (cdr system)))))))) ;System -> String


;;Funcion:get-system-logged
;;Descripcion: Retorna 1 si existe un usuario ogueado y 0 si no
;;Dom: system
;;REC:estado de systema con usuario logueado
(define get-system-logged (lambda (system) (car (cdr (cdr (cdr (cdr (cdr(cdr system)))))))));


;;Funcion:get-system-elementos
;;Descripcion: retorna el total de elementos del sistema
;;Dom: system
;;REC:elemenots del systema
(define get-system-elementos (lambda (system) (car (cdr (cdr (cdr (cdr (cdr(cdr(cdr system))))))))));


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Modificadores


;; Modificador TDA System


;;Funcion: system-add-drive
;;Descripcion: agregar nuevo drive (se usa en RF4 add-drive)
;;Dom:system X nuevo drive
;;REC:system con nueva unidad
(define system-add-drive
  (lambda (system new-drive)
    (make-system (get-system-name system)
                 (get-system-users system)
                 (cons new-drive (get-system-drives system))
                 (get-system-current-user system)
                 (get-system-current-drive system)
                 (get-system-current-path system)
                 (get-system-logged system)
                 (cons(make-elemento system (car new-drive) "U")(get-system-elementos system)))))


;;

;;Funcion: system-add-user
;;Descripcion:  agregar nuevo user (se usa en RF5 register)
;;Dom:system X nuevo user
;;REC:system con nuevo user
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

;;Funcion: system-make-login
;;Descripcion: hacer Login
;;Dom:system X usuario
;;REC:system con estado cambiado a logueado y nuevo usuario en currentuser
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


;;Funcion: system-make-logout
;;Descripcion: hacer Logout y limpia el currentuser
;;Dom:system 
;;REC:system con estado cambiado a no logueado
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



;;  TDA system - register

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



;; TDA system - login

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


;;;Loggout
;; Modificador ejecuta un suistema nuevocuando se hace logout
;; Dom: System 
;; Rec: System
(define logout
  (lambda (system)
    (if (= (get-system-logged system) 1)
        (system-make-logout system)
        system)))




;; RF3. TDA system - run
;;Ejecuta un comando sober el system
;; Dom: System X
;;      comando
;; Rec: System
(define (run system cmd)
  (cmd system))



;;TDA system - add-drive

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






































;;;;;;;;;;Creacion de carpetas

;;;modificador

;;Funcion: md
;;Descripcion: crea un nuevo elemento tipo directorio
;;Dom:system X folder
;;REC:System con nuevo elemento
(define md
  (lambda (system)
    (lambda (folder )
    (system-make-new-folder system folder))))

;;;Constructor
;;Funcion: system-make-new-folder
;;Descripcion: construye nuevo sistema con nuevo elemento agregado
;;Dom:system X folder
;;REC:System con nuevo elemento
(define system-make-new-folder
  (lambda (system folder)
           (make-system (get-system-name system)
                 (get-system-users system)       
                 (get-system-drives system)
                 (get-system-current-user system)
                 (get-system-current-drive system)
                 (get-system-current-path system)
                 (get-system-logged system)
                 (cons(make-elemento system folder "D")(get-system-elementos system)))))



























;;TDA elemento
;;;Constructor de elemento


;;;Constructor
;;Funcion: make-elemento
;;Descripcion: construye nuevo elemento
;;Dom:system X elemento X tipo
;;REC:nuevo elemento
(define make-elemento
  (lambda (system elemento t)
  (list (get-system-current-user system)
        t
        (get-system-current-drive system)
        (get-system-current-path system)
        elemento
        (system-make-path (get-system-current-path system) elemento t ))))
 
;;;Selector Elementos
;;;;;obtener elementos
;;Funcion:get-system-elements
;;Descripcion: retorne los elementos del sistema
;;Dom:system
;;REC:elementos
(define (get-system-elements system)
  (list-ref system 7))




;;Funcion:filtro-xuser
;;Descripcion:retorna los elementos filtrados por el currentuser
;;Dom:elementos X user
;;REC:Elementosxuser
(define (filtro-xuser listaelementos user)
  (filter (lambda (l) (string=? (car l) user)) listaelementos))


;;Funcion:filtro-xpath
;;Descripcion:retorna los elementos filtrados por el currentpath
;;Dom:elementos X path
;;REC:Elementosxpath
(define (filtro-xpath listaelementos path)
  (display path)
  (filter (lambda (l) (string=? (car(reverse l)) path)) listaelementos))
  
;;;;;;;;;;;;;;;;;;;;;Funcion CD
;;
;;
;;;








;;Funcion:cd
;;Descripcion:Decide si ejecuta entrada a directorio ruta, carpeta anterior o raiz segun argumento
;;Dom:system
;;REC:system con nuevo currentpath
(define cd
  (lambda (system)
     (lambda (func)
  (cond((string=? ".."  func)
        (dospuntos system)
        )
        ((string=? "/"  func) (slash system))    
        (else (cdfolder system func) 
        )))))


;;Funcion:dospuntos
;;Descripcion:REaliza un cambio de current path a la carpeta anterior
;;Dom:system
;;REC:system con nuevo currentpath      
(define dospuntos ( lambda (system)
  (display "dospuntossssss")))


;;Funcion:dospuntos
;;Descripcion:REaliza un cambio de current path a la raiz de la unidad actual
;;Dom:system
;;REC:system con nuevo currentpath   
(define slash ( lambda (system)
  (display "slashhhhhhhs")))

;;Funcion:cdfolder
;;Descripcion:cambia el current path por uno nuevo de una carpeta inmeiata
;;Dom:system X folder
;;REC:
(define (cdfolder system folder)
  (display
   (filtro-xpath
  (filtro-xuser (get-system-elements system)(get-system-current-user system))
  (get-system-current-drive system)
  )))



















;;;TDA Path
;;Funcion:system-make-path
;;Descripcion:realiza un path con los string de drive y current path para un nuevo elemento
;;Dom:path X elemento X tipo
;;REC:nuevo path para un elemento 
(define system-make-path
    (lambda (path elemento t)
      (if(string=? t "U")
         (string-append path ":" "/" (string elemento))
           (if(string=? t "D")
              ;(list->string (reverse (string->list (string-append elemento ":" "/" (string path )))))
              (string-append (string path) ":" "/"elemento)
              ((string-append path ":" "/" (string elemento) ))))))







;;Funcion:estamos-en-raiz
;;Descripcion:retorna true si ya estamos en la raiz de la unidad actual
;;Dom:system
;;REC:boolean 
(define (estamos-en-raiz system)
    (display (string=? (string(get-system-current-drive system))(string(get-system-current-path system)))))




;;Funcion:es-xuser?
;;Descripcion:retorna true si el current usaer es el mismo del elemento
;;Dom:elemetos
;;REC:boolean 
(define (es-xuser? elementos)
  (string=? (car(car(get-system-elements elementos))) (get-system-current-user system) ))
  
  
   
   
     

















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







