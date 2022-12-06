 :- use_module(library(pce)).
 :- pce_image_directory('./imagenes').
 :- use_module(library(pce_style_item)).
 :- dynamic color/2.

 resource(img_principal, image, image('img_principal.jpg')).
 resource(portada, image, image('portada.jpg')).
 resource(desequilibrio_nutricional, image, image('desequilibrio_nutricional.jpg')).
 resource(diarrea, image, image('diarrea.jpg')).
 resource(motilidad_gastrointestinal, image, image('motilidad_gastrointestinal.jpg')).
 resource(lo_siento_diagnostico_desconocido, image, image('desconocido.jpg')).
  resource(hipo, image, image('hipo.jpg')).
 resource(hiper, image, image('hiper.jpg')).
 resource(fragilidad_capilar, image, image('fragilidad_capilar.jpg')).
 resource(estre, image, image('estre.jpg')).
 resource(desh, image, image('desh.jpg')).
 resource(ausencia_flatulencias, image, image('ausencia_flatulencias.jpg')).
 resource(aceleracion_vaciado, image, image('aceleracion_vaciado.jpg')).
 resource(abdomen_distendido, image, image('abdomen_distendido.jpg')).
 resource(residuo_gastrico, image, image('residuo_gastrico.jpg')).
 resource(urgencia_intestinal, image, image('urgencia_intestinal.jpg')).
 resource(peso_bajo, image, image('peso_bajo.jpg')).
 resource(dificultad, image, image('dificultad.jpg')).
 resource(dolor_abdominal, image, image('dolor_abdominal.jpg')).
 resource(colico_abdominal, image, image('colico_abdominal.jpg')).
 resource(caida_excesiva, image, image('caida_excesiva.jpg')).
 resource(alteracion_sonidos, image, image('alteracion_sonidos.jpg')).

 mostrar_imagen(Pantalla, Imagen) :- new(Figura, figure),
                                     new(Bitmap, bitmap(resource(Imagen),@on)),
                                     send(Bitmap, name, 1),
                                     send(Figura, display, Bitmap),
                                     send(Figura, status, 1),
                                     send(Pantalla, display,Figura,point(100,80)).
  mostrar_imagen_tratamiento(Pantalla, Imagen) :-new(Figura, figure),
                                     new(Bitmap, bitmap(resource(Imagen),@on)),
                                     send(Bitmap, name, 1),
                                     send(Figura, display, Bitmap),
                                     send(Figura, status, 1),
                                     send(Pantalla, display,Figura,point(20,100)).
 nueva_imagen(Ventana, Imagen) :-new(Figura, figure),
                                new(Bitmap, bitmap(resource(Imagen),@on)),
                                send(Bitmap, name, 1),
                                send(Figura, display, Bitmap),
                                send(Figura, status, 1),
                                send(Ventana, display,Figura,point(0,0)).
  imagen_pregunta(Ventana, Imagen) :-new(Figura, figure),
                                new(Bitmap, bitmap(resource(Imagen),@on)),
                                send(Bitmap, name, 1),
                                send(Figura, display, Bitmap),
                                send(Figura, status, 1),
                                send(Ventana, display,Figura,point(500,60)).
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
  botones:-borrado,
                send(@boton, free),
                send(@btntratamiento,free),
                mostrar_diagnostico(Enfermedad),
                send(@texto, selection('El Diagnostico a partir de los datos es:')),
                send(@resp1, selection(Enfermedad)),
                new(@boton, button('Iniciar consulta',
                message(@prolog, botones)
                )),

                new(@btntratamiento,button('Detalles y Tratamiento',
                message(@prolog, mostrar_tratamiento,Enfermedad)
                )),
                send(@main, display,@boton,point(20,450)),
                send(@main, display,@btntratamiento,point(138,450)).



  mostrar_tratamiento(X):-new(@tratam, dialog('Tratamiento')),
                          send(@tratam, append, label(nombre, 'Explicacion: ')),
                          send(@tratam, display,@lblExp1,point(70,51)),
                          send(@tratam, display,@lblExp2,point(50,80)),
                          tratamiento(X),
                          send(@tratam, transient_for, @main),
                          send(@tratam, open_centered).

tratamiento(X):- send(@lblExp1,selection('De Acuerdo Al Diagnostico El Tratamiento Es:')),
                 mostrar_imagen_tratamiento(@tratam,X).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


   preguntar(Preg,Resp):-new(Di,dialog('Colsultar Datos:')),
                        new(L2,label(texto,'Responde las siguientes preguntas')),
                        id_imagen_preg(Preg,Imagen),
                        imagen_pregunta(Di,Imagen),
                        new(La,label(prob,Preg)),
                        new(B1,button(si,and(message(Di,return,si)))),
                        new(B2,button(no,and(message(Di,return,no)))),
                        send(Di, gap, size(25,25)),
                        send(Di,append(L2)),
                        send(Di,append(La)),
                        send(Di,append(B1)),
                        send(Di,append(B2)),
                        send(Di,default_button,'si'),
                        send(Di,open_centered),get(Di,confirm,Answer),
                        free(Di),
                        Resp=Answer.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  interfaz_principal:-new(@main,dialog('Sistema Experto Diagnosticador de Enfermedades deL Goldfish',
        size(1000,1000))),
        new(@texto, label(nombre,'El Diagnostico a partir de los datos es:',font('times','roman',18))),
        new(@resp1, label(nombre,'',font('times','roman',22))),
        new(@lblExp1, label(nombre,'',font('times','roman',14))),
        new(@lblExp2, label(nombre,'',font('times','roman',14))),
        new(@salir,button('SALIR',and(message(@main,destroy),message(@main,free)))),
        new(@boton, button('Iniciar consulta',message(@prolog, botones))),

        new(@btntratamiento,button('¿Tratamiento?')),

        nueva_imagen(@main, img_principal),
        send(@main, display,@boton,point(138,450)),
        send(@main, display,@texto,point(20,130)),
        send(@main, display,@salir,point(300,450)),
        send(@main, display,@resp1,point(20,180)),
        send(@main,open_centered).

       borrado:- send(@resp1, selection('')).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  crea_interfaz_inicio:- new(@interfaz,dialog('Bienvenido al Sistema Experto Diagnosticador',
  size(1000,1000))),

  mostrar_imagen(@interfaz, portada),

  new(BotonComenzar,button('COMENZAR',and(message(@prolog,interfaz_principal) ,
  and(message(@interfaz,destroy),message(@interfaz,free)) ))),
  new(BotonSalir,button('SALIDA',and(message(@interfaz,destroy),message(@interfaz,free)))),
  send(@interfaz,append(BotonComenzar)),
  send(@interfaz,append(BotonSalir)),
  send(@interfaz,open_centered).

  :-crea_interfaz_inicio.
  
  
  
  conocimiento('desequilibrio_nutricional',
['colico abdominal', 'dolor abdominal',
'peso corporal por debajo del rango de peso ideal para la edad y sexo','fragilidad capilar','estreñimiento','caida excesiva de cabello','hiperperistaltismo','hipoglucemia']).

conocimiento('diarrea',
['colico abdominal', 'dolor abdominal',
'urgencia intestinal','deshidratacion','hiperperistaltismo']).

conocimiento('motilidad_gastrointestinal',['colico abdominal',
'dolor abdominal', 'ausencia de flatulencias','aceleracion en el vaciado gastrico','alteracion de los osnidos intestinales','residuo gastrico de color bilioso','dificultad para defecar','abdomen distendido']).


id_imagen_preg('colico abdominal','colico_abdominal').
id_imagen_preg('dolor abdominal','dolor_abdominal').
id_imagen_preg('peso corporal por debajo del rango de peso ideal para la edad y sexo','peso_bajo').
id_imagen_preg('urgencia intestinal','urgencia_intestinal').
id_imagen_preg('ausencia de flatulencias','ausencia_flatulencias').
id_imagen_preg('fragilidad capilar','fragilidad_capilar').
id_imagen_preg('deshidratacion','desh').
id_imagen_preg('hiperperistaltismo','hiper').
id_imagen_preg('estreñimiento','estre').
id_imagen_preg('caida excesiva de cabello','caida_excesiva').
id_imagen_preg('hipoglucemia','hipo').
id_imagen_preg('aceleracion en el vaciado gastrico','aceleracion_vaciado').
id_imagen_preg('alteracion de los sonidos intestinales','alteracion_sonidos').
id_imagen_preg('residuo gastrico de color bilioso','residuo_gastrico').
id_imagen_preg('dificultad para defecar','dificultad').
id_imagen_preg('abdomen distendido','abdomen_distendido').

 :- dynamic conocido/1.

  mostrar_diagnostico(X):-haz_diagnostico(X),clean_scratchpad.
  mostrar_diagnostico(lo_siento_diagnostico_desconocido):-clean_scratchpad .

  haz_diagnostico(Diagnosis):-
                            obten_hipotesis_y_sintomas(Diagnosis, ListaDeSintomas),
                            prueba_presencia_de(Diagnosis, ListaDeSintomas).


obten_hipotesis_y_sintomas(Diagnosis, ListaDeSintomas):-
                            conocimiento(Diagnosis, ListaDeSintomas).


prueba_presencia_de(Diagnosis, []).
prueba_presencia_de(Diagnosis, [Head | Tail]):- prueba_verdad_de(Diagnosis, Head),
                                              prueba_presencia_de(Diagnosis, Tail).


prueba_verdad_de(Diagnosis, Sintoma):- conocido(Sintoma).
prueba_verdad_de(Diagnosis, Sintoma):- not(conocido(is_false(Sintoma))),
pregunta_sobre(Diagnosis, Sintoma, Reply), Reply = 'si'.


pregunta_sobre(Diagnosis, Sintoma, Reply):- preguntar(Sintoma,Respuesta),
                          process(Diagnosis, Sintoma, Respuesta, Reply).


process(Diagnosis, Sintoma, si, si):- asserta(conocido(Sintoma)).
process(Diagnosis, Sintoma, no, no):- asserta(conocido(is_false(Sintoma))).


clean_scratchpad:- retract(conocido(X)), fail.
clean_scratchpad.


conocido(_):- fail.

not(X):- X,!,fail.
not(_).
