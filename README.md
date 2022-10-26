# TRABAJO FUNCIONES INCERTIDUMBRE: GRUPO 1.
 
 ## Modificaciones código:
 
 * *criterio.Todos*
 
Hemos añadido una nueva columna a la función que da para cada alternativa el número de veces que es óptima entre todos los criterios. Este cambio tiene en cuenta los empates de manera que pueda haber un mismo conteo en más de una alternativa.
Además, en el pie de la columna se ha dado la alternativa que finalmente resulta óptima (la que reune un conteo mayor) en caso de que sea solo una, o varias separadas por comas en caso contrario.

En cuanto a la estética de la tabla resultante de esta función se han añadido los siguientes cambios: añadido cabecera, modificado la fuente de la letra, añadido marcos, 
hecho responsiva y dado la cualidad de que al pasar el ratón sobre cualquiera de sus filas esta se sombree en amarillo.
 
 ## Probar funciones y modificaciones:
 
 Se ha creado un documento *"Rmarkdown"* en el cual resolveremos ejercicios de la relación con la finalidad de probar las modificaciones que realicemos en el código.
 
 Se ha comprobado que la función criterio.Todos actualizada funciona correctamente en todos los ejercicios.
 
 ## Comentar funciones:
 
Se han repartido las 15 funciones pertenecientes al script *"funcionesincertidumbre.R"* de forma que cada uno de los integrantes debemos comentar 3 de ellas. Indicaremos que hace cada comando y, en caso de haberlas, las funciones teóricas que se están aplicando.

* Lista de funciones comentadas:

  - crea.tablaX
  - which.max.general
  - criterio.Wald
  - criterio.Hurwicz
  - dibuja.criterio.Hurwicz_Intervalos 
  - criterio.Laplace
  
 ## Problemas propuestos:
 
Se ha creado un documento *"Rmarkdown"* en el cual cada componente del grupo propondrá un enunciado de un problema y su
resolución empleando todos los métodos de incertidumbre.

## Wiki:

Cada repositorio en GitHub.com viene equipado con una sección para alojar documentación, llamada _Wiki_. Puedes usar la wiki de tu repositorio para decir cómo has 
diseñado tu proyecto o sus principios básicos. En nuestro caso se ha creado una Wiki a modo de presentación para que todo aquel que acceda al repositorio conozca
en que consiste el trabajo y que se espera de el. 

La wiki incluye:
* Home: breve presentación.
* Funciones incertidumbre: Enunciado del trabajo.
* Integrantes del Grupo 1

### Diferencia _README_ y _Wiki_:

Un archivo README dice rápidamente lo que tu proyecto puede hacer (en nuestro caso que cambios o añadidos hemos hecho), mientras que puedes utilizar la _Wiki_ para 
proporcionar documentación adicional (como información más específica asociada a nuestro grupo).
