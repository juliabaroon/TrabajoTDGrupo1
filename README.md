# TRABAJO FUNCIONES INCERTIDUMBRE: GRUPO 1.
 
## Modificaciones código:
 
 * *criterio.Todos*
 
Hemos añadido una nueva columna a la función que da para cada alternativa el número de veces que es óptima entre todos los criterios. Este cambio tiene en cuenta los
empates de manera que pueda haber un mismo conteo en más de una alternativa.
Además, en el pie de la columna se ha dado la alternativa que finalmente resulta óptima (la que reune un conteo mayor) en caso de que sea solo una, o varias separadas
por comas en caso contrario.

En cuanto a la estética de la tabla resultante de esta función se han añadido los siguientes cambios: añadido cabecera, modificado la fuente de la letra, añadido marcos y bordes entre filas y columnas, centrado valores, hecho responsiva y dado la cualidad de que al pasar el ratón sobre cualquiera de sus filas esta se sombree en amarillo.

Se ha probado a añadir como cambio estétito el sombreado en verde de aquellas alternativas que resulten óptimas (con un conteo superior en la columna _"Veces óptimo"_)
y de rojo aquellas que no, funcionando tanto en casos ordinarios como con empates. Este cambio se ha propuesto mediante un _"Pull request"_ habiendo hecho previamente un _"Fork"_ del repositorio.

  * *crea.tablaX*
  
  Se ha dado la posibilidad de proporcionar los nombres de las alternativas en la función de manera que las filas de la tabla tengan un nombre asociado.
 
 
 ## Probar funciones y modificaciones:
 
 Se ha creado un documento *"Rmarkdown"* en el cual resolveremos ejercicios de la relación con la finalidad de probar las modificaciones que realicemos en el código.
 
 Se ha comprobado que la función _criterio.Todos_ actualizada funciona correctamente en todos los ejercicios.
 
 Se ha comprobado que los cambios estéticos de la función _criterio.Todos_ se ven correctamente en todos los casos.
 
  * *crea.tablaX*
  
  Se ha comprobado que tanto como si se dan o no nombres a las alternativas la función trabaja correctamente.
  
## Comentar funciones:
 
Se han repartido las 15 funciones pertenecientes al script *"funcionesincertidumbre.R"* de forma que cada uno de los integrantes debemos comentar 3 de ellas. Indicaremos que hace cada comando y, en caso de haberlas, las funciones teóricas que se están aplicando.

* Lista de funciones comentadas:

  - _crea.tablaX_
  - _which.max.general_
  - _criterio.Wald_
  - _criterio.Hurwicz_
  - _dibuja.criterio.Hurwicz_Intervalos_
  - _criterio.Laplace_
  - _distanciaEuclidea_
  - _criterio.Hurwicz.General_
  - _criterio.PuntoIdeal_
  
## Problemas propuestos:
 
Se ha creado un documento *"Rmarkdown"* en el cual cada componente del grupo propondrá un enunciado de un problema y su
resolución empleando todos los métodos de incertidumbre.

## Paquete R:

Se ha creado un nuevo proyecto dentro de la subcarpeta _"FuncionesIncertidumbre"_ para crear un nuevo paquete de R el cual contenga todas las funciones creadas en el 
fichero _funcionesncertidumbre.R_.

Además se ha añadido una vignette asocidada a dicho paquete llamada _"my-vignette"_ en la cual redactaremos todo lo que el paquete es capaz de hacer y ejemplos de aplicación para que cualquier persona que lo consulte pueda ver como funciona fácilmente.

## Wiki:

Cada repositorio en GitHub.com viene equipado con una sección para alojar documentación, llamada _Wiki_. Puedes usar la wiki de tu repositorio para decir cómo has 
diseñado tu proyecto o sus principios básicos. En nuestro caso se ha creado una Wiki a modo de presentación para que todo aquel que acceda al repositorio conozca
en que consiste el trabajo y que se espera de el. 

La Wiki incluye:
* **Home**: breve presentación.
* **Funciones incertidumbre**: Enunciado del trabajo.
* **Integrantes del Grupo 1**
* **Reparto funciones a comentar**: enlazo el issue en el que repartimos las tareas para que sea de fácil acceso para todo el que lo quiera consultar.

### Diferencia _README_ y _Wiki_:

Un archivo _README_ dice rápidamente lo que tu proyecto puede hacer (en nuestro caso que cambios o añadidos hemos hecho), mientras que puedes utilizar la _Wiki_ para 
proporcionar documentación adicional (como información más específica asociada a nuestro grupo).
