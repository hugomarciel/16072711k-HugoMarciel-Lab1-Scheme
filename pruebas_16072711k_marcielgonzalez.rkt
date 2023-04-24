#lang racket
(define S0 (system "newSystem"))

;añadiendo unidades. Incluye caso S2 que intenta añadir unidad con una letra que ya existe
(define S1 ((run S0 add-drive) #\C "SO" 1000))
(define S2 ((run S1 add-drive) #\C "SO1" 3000))
(define S3 ((run S2 add-drive) #\D "Util" 2000))


(define S4 ((run S3 add-drive) #\P "Unix" 5000))
(define S5 ((run S4 add-drive) #\L "debian" 1000))



;añadiendo usuarios. Incluye caso S6 que intenta registrar usuario duplicado
(define S6 ((run S5 register) "user1"))
(define S7 ((run S6 register) "user1"))
(define S8 ((run S7 register) "user2"))

(define S9 ((run S8 register) "user4")) 
(define S10 ((run S9 register) "user6"))        

;iniciando sesión con usuarios. Incluye caso S8 que intenta iniciar sesión con user2 sin antes haber salido con user1
(define S11 ((run S10 login) "user1"))
(define S12 ((run S11 login) "user2"))

(define S13 (run S12 logout))
(define S14 ((run S13 login) "user6"))


;cerrando sesión user1 e iniciando con user4
(define S15 (run S14 logout))
(define S16 ((run S15 login) "user4"))



;cambios de unidad, incluyendo unidad inexistente K
(define S17 ((run S16 switch-drive) #\K))
(define S18 ((run S17 switch-drive) #\C))

;añadiendo carpetas. Incluye casos de carpetas duplicadas.
(define S19 ((run S18 md) "folder1"))
(define S20 ((run S19 md) "folder2"))
(define S21 ((run S20 md) "folder5"))
(define S22 ((run S21 md) "folder3"))