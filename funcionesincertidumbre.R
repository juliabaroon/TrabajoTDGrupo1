# fichero: teoriadecision_funciones_incertidumbre_nuevo.R ----
## Funciones útiles ----

crea.tablaX = function(vector_matporfilas,numalternativas=3,numestados=4) { #le proporcionamos a la función un vector, el número de alternativas y el número de estados

    X = matrix(vector_matporfilas,nrow=numalternativas,ncol=numestados,byrow=TRUE) # creamos una matriz a partir del vector dado de dimensión (numalternativasxnumestados)
    colnames(X) = paste('e',1:numestados,sep=''); # le asignamos a las columnas los nombres de los estados
    rownames(X) = paste('d',1:numalternativas,sep=''); # le asignamos a las filas los nombres de las alternativas
    return(X); # nos devuelve la matriz en forma de tabla

}

# Introducimos los datos en R en forma de matriz:
#   ```{r}
# X = matrix(c(5,4,6,2,3,1,-1,8,7,5,2,0),nrow=4,ncol=3,byrow=TRUE)
# X
# colnames(X)=c('e1','e2','e3')
# rownames(X)=c('d1','d2','d3','d4')
# X
# ```



which.min.general = function(vector) {
    minimo = min(vector);
    res = which(vector == minimo);
    return(res);

}

which.max.general = function(vector) {#proporcionamos un vector a la función
    maximo = max(vector);#calculamos el valor máximo del mismo (valor)
    res = which(vector == maximo);#en que posición del vector se encuentra el valor máximo
    return(res);#devolvemos la posición obtenida
}


##which.min.general(c(3,2,8,2,9,2))
##which.max.general(c(3,2,8,1,9,2)) #ejemplo de aplicación

distanciaEuclidea = function(pto1,pto2) {
    return( sqrt( sum( (pto1-pto2)^2 )  ) )
}
#trata de enontrar la distancia entre dos puntos, dados dichos puntos en R.
#Será la dimensión igual al número de valores que introduzcamos en los vectores llamados pto1 y pto2.
#se trata de restar los dos valores de x en caso de que el vector sea (x,y) y elevarlos al cuadrado
# sumarle la misma cuenta realizada para los valores de la y, y a todo esto le hacemos la raiz cuadrada,
# quedandonos la distancia entre ambos.


criterio.tablaX.ejemplos = function(cual=1) {

    if (cual==2) { ## cual == 2  ## desfav.
        X = crea.tablaX(c(2,12,-3,5,5,-1,0,10,-2),numalternativas = 3,numestados = 3)
    } else if (cual==3) { ## cual == 3  ## desfav.
        X = crea.tablaX(c(125,120,156,60,130,80),numalternativas = 3,numestados = 2)
    } else {  ## cual == 1
        X = crea.tablaX(c(5,4,6,2,3,1,-1,8,7,5,2,0),numalternativas = 4,numestados = 3)
    }
    return(X);

}

## Funciones Métodos de Decisión bajo Incertidumbre ----

## Criterio de Wald o Pesimista

criterio.Wald = function(tablaX,favorable=TRUE) {
# le proporcionamos a la función una tabla de decisión (ya creada con la función crea.tablaX)
# e indicamos si la matriz es de beneficios y maximizamos (favorable=T)
# o si es de costos y minimizamos (favorable=F)
    X = tablaX;
    if (favorable) { # caso favorable (beneficios)
        AltW = apply(X,MARGIN=1,min); #  mínimo por filas
        ##AltW
        Wald = max(AltW); # seleccionamos el máximo de los mínimos
        Alt_Wald = which.max.general(AltW); # nos indica la posición del máximo
        metodo = 'favorable';
    } else { # caso desfavorable (costos)
        AltW = apply(X,MARGIN=1,max); #  máximo por filas
        ##AltW
        Wald = min(AltW); # seleccionamos el mínimo de los máximos
        Alt_Wald = which.min.general(AltW); # nos indica la posición del mínimo
        metodo = 'desfavorable';
    }
    resultados = list();
    resultados$criterio = 'Wald'; # nombre del criterio
    resultados$metodo = metodo; # favorable o desfavorable
    resultados$tablaX = tablaX; # tabla de decisión
    resultados$ValorAlternativas = AltW; # valor de las alternativas
    resultados$ValorOptimo = Wald; # valor de la alternativa óptima
    resultados$AlternativaOptima = Alt_Wald; # alternativa óptima

    return(resultados); # nos devuelve la lista con los resultados


}


## Optimista

criterio.Optimista = function(tablaX,favorable=TRUE) {

    X = tablaX;
    if (favorable) {
        AltM = apply(X,MARGIN=1,max);
        ##AltM
        Maximax = max(AltM);
        Alt_Maximax = which.max.general(AltM);
        metodo = 'favorable';
    } else {
        AltM = apply(X,MARGIN=1,min);
        ##AltM
        Maximax = min(AltM);
        Alt_Maximax = which.min.general(AltM);
        metodo = 'desfavorable';
    }
    resultados = list();
    resultados$criterio = 'Optimista';
    resultados$metodo = metodo;
    resultados$tablaX = tablaX;
    resultados$ValorAlternativas = AltM;
    resultados$ValorOptimo = Maximax;
    resultados$AlternativaOptima = Alt_Maximax;

    return(resultados);


}


## Hurwicz


# factor de optimismo   (alfab * "lo mejor" Altmax en favor. y Altmin en desf.)
criterio.Hurwicz = function(tablaX,alfa=0.3,favorable=TRUE) {
    #se le da a la función una tabla tal que las columnas son los estados de la natulareza y las filas las alternativas
    # alfa es un escalar entre 0 y 1 que pondera el criterio óptimista, luego cuanto más cercano a 1 más optimista se es y viceversa
    # la solucióna asociada al criterio de Hurwicz se obtiene para ese único valor de alfa
    #Y si es favorable o no, es decir, si la tabla esta asociada a beneficios (buscaríamos maximizar) o a costos (minimizar)
    #
    X = tablaX;#tabla de decisión
    if (favorable) {#si son beneficios
        Altmin = apply(X,MARGIN=1,min);#mínimo por filas
        Altmax= apply(X,MARGIN=1,max);#máximo por filas
        AltH = alfa * Altmax + (1-alfa) * Altmin #fórmula criterio: alfa*Optimista + (1-alfa)Pesimista
        Hurwicz = max(AltH)#máximo de los beneficios=óptimo (alternativa que maximiza el beneficio)
        Alt_Hurwicz = which.max.general(AltH)#en que posició se encuentra el óptimo
        metodo = 'favorable'; #método usado
    } else {#si la tabla de decisión es de costos
        Altmin = apply(X,MARGIN=1,min);#mínimo por filas
        Altmax= apply(X,MARGIN=1,max);#máximo por filas
        AltH = (1-alfa) * Altmax + alfa * Altmin #fórmula criterio: alfa*Pesimista + (1-alfa)Optimista
        Hurwicz = min(AltH) #menor costo=óptimo
        Alt_Hurwicz = which.min.general(AltH)#en que posición se encuentra el óptimo (alternativa que minimiza el costo)
        metodo = 'desfavorable';#método usado
    }
    resultados = list();#lista vacía en la que almacenar los resultados
    resultados$criterio = 'Hurwicz'; #nombre del criterio
    resultados$alfa = alfa;#alfa usado
    resultados$metodo = metodo; #método favorable o desfavorable
    resultados$tablaX = tablaX;#tabla de decisión
    resultados$ValorAlternativas = AltH;#valores asociados a las alternativas
    resultados$ValorOptimo = Hurwicz;#valor asociado a la alternativa óptima
    resultados$AlternativaOptima = Alt_Hurwicz;#nombre de la alternativa óptima

    return(resultados);#devolvemos la lista de resultados


}

## factor de optimismo   (alfab * "lo mejor" Altmax en favor. y Altmin en desf.)
criterio.Hurwicz.General = function(tablaX,alfa=0.3,favorable=TRUE) {
    # si alfa es un escalar entre 0 y 1 lo obtiene para ese único valor
    # si alfa es igual a un número mayor que 1, lo usa para obtener cálculos para dividir el rango 0-1
    X = tablaX;
    if (favorable) {
        Altmin = apply(X,MARGIN=1,min);
        Altmax= apply(X,MARGIN=1,max);
        if (alfa<=1) {
            valfa = c(alfa);
        } else {
            valfa = seq(from=0,to=1,by=(1/alfa)); ## alfa: 100, 200,
        }
        vHurwicz = rep(0,length(valfa))
        Alt_vHurwicz = rep(0,length(valfa))
        for (i in 1:length(valfa)) {
            alfab = valfa[i];
            vAltH = alfab * Altmax + (1-alfab) * Altmin;
            vHurwicz[i] = max(vAltH);
            Alt_vHurwicz[i] = which.max(vAltH);
            Alt_vHurwicz_g = which.max.general(vAltH);
        }
        metodo = 'favorable';
    } else {
        Altmin = apply(X,MARGIN=1,min);
        Altmax= apply(X,MARGIN=1,max);
        if (alfa<=1) {
            valfa = c(alfa);
        } else {
            valfa = seq(from=0,to=1,by=(1/alfa)); ## alfa: 100, 200,
        }
        vHurwicz = rep(0,length(valfa))
        Alt_vHurwicz = rep(0,length(valfa))
        for (i in 1:length(valfa)) {
            alfab = valfa[i];
            vAltH = (1-alfab) * Altmax + alfab * Altmin;
            vHurwicz[i] = min(vAltH);
            Alt_vHurwicz[i] = which.min(vAltH);
            Alt_vHurwicz_g = which.min.general(vAltH);

        }
        metodo = 'desfavorable';
    }
    resultados = list();
    resultados$criterio = 'Hurwicz';
    resultados$alfa = alfa;
    resultados$metodo = metodo;
    resultados$tablaX = tablaX;
    resultados$ValorAlternativas = vAltH;
    resultados$ValorOptimo = vHurwicz;
    if (length(valfa)==1) {
        resultados$AlternativaOptima = Alt_vHurwicz_g;
    } else {
        resultados$AlternativaOptima = Alt_vHurwicz;
    }

    return(resultados);



}



dibuja.criterio.Hurwicz = function(tablaX,favorable=TRUE) {
    X = tablaX;
    Altmin = apply(X,MARGIN=1,min);
    Altmax = apply(X,MARGIN=1,max);
    valfa = seq(from=0,to=1,by=0.05);
    vHurwicz = rep(0,length(valfa));
    Alt_vHurwicz = rep(0,length(valfa));
    for (i in 1:length(valfa)) {
        alfab = valfa[i];
        if (favorable) {
            vAltH = alfab * Altmax + (1-alfab) * Altmin;
            vHurwicz[i] = max(vAltH)
        } else {
            vAltH = alfab * Altmin + (1-alfab) * Altmax;
            vHurwicz[i] = min(vAltH)
        }

    }

    x0=0;x1=1;
    y0 = min(Altmin);
    y1 = max(Altmax);
    rg = y1-y0;
    y0=y0-0.1*rg;y1=y1+0.1*rg;
    plot(c(x0,x1), c(y0,y1), type = "n", xlab = "alpha", ylab = "Criterio Hurwicz");
    nn = length(Altmin);
    colores = rainbow(nn);
    abline(v=0);
    abline(v=1);
    if (favorable) {
        for (i in 1:nn) {
            aa = Altmin[i];
            bb = (Altmax[i] - Altmin[i]);
            abline(a=aa,b=bb,col=colores[i]);
        }
    } else {
        for (i in 1:nn) {
            aa = Altmax[i];
            bb = (Altmin[i] - Altmax[i]);
            abline(a=aa,b=bb,col=colores[i]);
        }
    }
    lines(valfa,vHurwicz,col=rainbow(nn+1)[nn+1],lty=3,lwd=3)
    if (favorable) {
        legend("bottomright",legend=rownames(X),fill=colores,inset=0.05)
        title("Criterio de Hurwicz (favorable - línea discontinua)")
    } else {
        legend("topright",legend=rownames(X),fill=colores,inset=0.05)
        title("Criterio de Hurwicz (desfavorable - línea discontinua)")
    }

}


# FUNCION : esta funcion nos da los valores de alfa para los que las alternativas cambian

# Entrada: Tabla, favorable (T/F)
# Salida: Intervalo -> Alternativa (óptima para ese intervalo de alfa)
# Autores: Ana Solis, Luca Ricardi y Paula Gutiérrez (Noviembre-2021)

dibuja.criterio.Hurwicz_Intervalos = function(tablaX,favorable=TRUE,mostrarGrafico=TRUE) {
    # le proporcionamos a la función una tabla de decisión (ya creada con la función crea.tablaX)
    # e indicamos si la matriz es de beneficios y maximizamos (favorable=T)
    # o si es de costos y minimizamos (favorable=F)
    # además indicamos si queremos que nos aparezca el gráfico cuando usemos la función

    X = tablaX # renombramos la tabla
    Altmin = apply(X,MARGIN=1,min)      # vector de minimos (por filas)
    Altmax = apply(X,MARGIN=1,max)      # vector de maximos (por filas)
    valfa = seq(from=0,to=1,by=0.05)    # vector de valores para alfa (entre 0 y 1)
    Hurw <- data.frame(Alt_opt = rep(0,length(valfa)),vHurwicz = rep(0,length(valfa)))

    #Alt_opt = rep(0,length(valfa))      # creamos el vector de decisiones (por el criterio de Hurwicz) para cada valor de alfa

    alfaCorte=c()                       # vector que contiene los valores de alfa donde cambian las decisiones
    for (i in 1:length(valfa)) {
        Opt <- criterio.Hurwicz(X, alfa = valfa[i], favorable)
        Hurw[i,] <-  rbind(Opt$AlternativaOptima[[1]],Opt$ValorOptimo) # obtenemos las alternativas para cada alfa
        Alt=c() # Este va a ser el Vector de las alternativas optimas para todos los alfa
        for (i in 1:dim(Hurw)[1]) {
            valrepetidos = duplicated(Hurw$Alt_opt) # Vector de TRUE/FALSE donde los FALSE son los elementos que se repiten
            if (isFALSE(valrepetidos[i])){
                Alt = c(Alt,Hurw$Alt_opt[i]) # Si es falso (si el valor se repite) lo almacenamos en el vector Alt
            }
        }
    }
    # Teniendo el vector de alternativas (Alt) buscamos los puntos de corte de las rectas asociadas a cada alternativa (beneficios)
    # Por ejemplo, la recta que sale de la alternativa a1 y a2 seria:
    #
    #               a1Max *alfa +(1-alfa)*a1Min = a2Max *alfa +(1-alfa)*a2Min
    #
    # Pasando todo a un  miembro e igualando a 0 nos queda:
    #
    #               alfa * (a1Max- a2Max - a1Min + a2Min) + a1Min -a2Min = 0
    #
    # Buscamos ahora los valores de alfa para los que se cortan las rectas asociadas a cada decision
    for (i in 1:(length(Alt)-1)){
        imax = as.numeric(Altmax[Alt[i]])      # maximo asociado a la decision i del vector Alt
        imax1 = as.numeric(Altmax[Alt[i+1]])   # maximo asociado a la decision i+1 del vector Alt
        imin = as.numeric(Altmin[Alt[i]])      # minimo asociado a la decision i del vector Alt
        imin1 = as.numeric(Altmin[Alt[i+1]])   # minimo asociado a la decision i+1 del vector Alt
        if (favorable){
            pCorte = function(alfa) {alfa * (imax-imax1-imin+imin1)+imin-imin1}
            alfaC = uniroot(pCorte, interval = c(0,1))$root[[1]] # Buscamos los 0 para cada funcion
            alfaCorte[i] = alfaC  # Almacenamos los valores de alfa para los que las rectas se cortan en alfaCorte
        } else {
            # Para el caso de costes (alternativas a1 y a2):
            #
            #               a1Max *(1-alfa) +alfa*a1Min = a2Max *(1-alfa) +alfa*a2Min
            #
            # Pasando todo a un  miembro e igualando a 0 nos queda:
            #
            #               alfa * (a1Min- a2Min - a1Max + a2Max) + a1Max -a2Max = 0
            #
            pCorte = function(alfa) {alfa * (imin-imin1-imax+imax1)+imax-imax1}
            alfaC = uniroot(pCorte, interval = c(0,1))$root[[1]] # Buscamos los 0 para cada funcion
            alfaCorte[i] = alfaC  # Almacenamos los valores de alfa para los que las rectas se cortan en alfaCorte
        }

    }

    if (mostrarGrafico) {
        x0=0;x1=1;
        y0 = min(Altmin); # mínimo del vector de los mínimos
        y1 = max(Altmax); # máximo del vector de los máximos
        rg = y1-y0;
        y0=y0-0.1*rg;y1=y1+0.1*rg;
        plot(c(x0,x1), c(y0,y1), type = "n", xlab = "alpha", ylab = "Criterio Hurwicz");
        nn = length(Altmin);
        colores = rainbow(nn)
        abline(v=0);
        abline(v=1);
        if (favorable) {
            for (i in 1:nn) {
                aa = Altmin[i];
                bb = (Altmax[i] - Altmin[i]);
                abline(a=aa,b=bb,col=colores[i]);
            }
        } else {
            for (i in 1:nn) {
                aa = Altmax[i];
                bb = (Altmin[i] - Altmax[i]);
                abline(a=aa,b=bb,col=colores[i]);
            }
        }

        lines(valfa,Hurw$vHurwicz,col="green",lty=3,lwd=3)
        abline(v = alfaCorte, col="red")

        if (favorable) {
            legend("bottomright",legend=rownames(X),fill=colores,inset=0.05) #leyendas añadidas
            title("Criterio de Hurwicz (favorable - línea discontinua)")
        } else {
            legend("topright",legend=rownames(X),fill=colores,inset=0.05) #leyendas añadidas
            title("Criterio de Hurwicz (desfavorable - línea discontinua)")
        }
    }

    alfaCorte = round(alfaCorte, 3)
    if (length(alfaCorte)==1){
        Int1=paste("(",0,",",alfaCorte,")")
        Int2=paste("(",alfaCorte,",",1,")")
        Soluciones = cbind(c(Int1,Int2),c(Alt[1],Alt[2]))
    } else {
        Int0=paste("(",0,",",alfaCorte[1],")")
        Int1=paste("(",alfaCorte[length(alfaCorte)],",",1,")")
        Int = ""
        Soluciones= c(Int0, Alt[1])
        for (i in 1:(length(alfaCorte)-1)){
            Int[i] = paste("(",alfaCorte[i],",",alfaCorte[i+1],")")
            Soluciones = rbind(Soluciones,c(Int[i],Alt[i+1]))
        }
        Soluciones = rbind(Soluciones,c(Int1,Alt[length(Alt)]))
    }
    colnames(Soluciones)=c("Intervalo","Alternativa")

    resultados = list();
    resultados$AltOptimas = Alt;
    resultados$PuntosDeCorte = alfaCorte;
    resultados$IntervalosAlfa = Soluciones;
    return(resultados)

}




## Savage

criterio.Savage = function(tablaX,favorable=TRUE) {

    X = tablaX;
    if (favorable) {
        Mejores = apply(X,MARGIN=2,max);
        temp1 = rep(Mejores,dim(X)[1])
        Mmejores = matrix(temp1,nrow=dim(X)[1],ncol=dim(X)[2],byrow=TRUE);
        Pesos = abs(Mmejores-X);
        ##print(Pesos)
        ## Ahora criterio Wald Minimax Pesimista (desfavorable)
        AltWS= apply(Pesos,MARGIN=1,max);
        Savage = min(AltWS);
        Alt_Savage = which.min.general(AltWS);
        metodo = 'favorable';
    } else {
        Mejores = apply(X,MARGIN=2,min);
        temp1 = rep(Mejores,dim(X)[1])
        Mmejores = matrix(temp1,nrow=dim(X)[1],ncol=dim(X)[2],byrow=TRUE);
        Pesos = abs(Mmejores-X);
        ## Ahora criterio Wald Minimax (desfavorable)
        AltWS= apply(Pesos,MARGIN=1,max);
        Savage = min(AltWS);
        Alt_Savage = which.min.general(AltWS);
        metodo = 'desfavorable';
    }
    resultados = list();
    resultados$criterio = 'Savage';
    resultados$metodo = metodo;
    resultados$tablaX = tablaX;
    resultados$Mejores = Mejores;
    resultados$Pesos = Pesos;
    resultados$ValorAlternativas = AltWS;
    resultados$ValorOptimo = Savage;
    resultados$AlternativaOptima = Alt_Savage;

    return(resultados);


}


## Laplace

criterio.Laplace = function(tablaX,favorable=TRUE) {#damos la tabla de decisión y si manejamos beneficios (favorable) o costos (desfavorable)

    X = tablaX; #tabla de decisión
    if (favorable) {#si son beneficios
        AltL = apply(X,MARGIN=1,mean);#promedio por filas
        Laplace = max(AltL) # favorable (mayor promedio= valor óptimo)
        Alt_Laplace = which.max.general(AltL)#que alternativa tiene asociada el valor óptimo
        metodo = 'favorable'; #beneficios
    } else {
        AltL = apply(X,MARGIN=1,mean) #promedio por filas
        Laplace = min(AltL) # desfavorable (menor promedio = valor óptimo)
        Alt_Laplace = which.min.general(AltL) #que alternativa tiene asociada el valor óptimo
        metodo = 'desfavorable'; #costos
    }
    resultados = list(); #creamos lista vacía en la que almacenar los resultados
    resultados$criterio = 'Laplace'; #nombre del criterio
    resultados$metodo = metodo; #beneficios o costos
    resultados$tablaX = tablaX; #tabla de decisión
    resultados$ValorAlternativas = AltL; #promedios por fila
    resultados$ValorOptimo = Laplace; #valor asociado a la alternativa óptima
    resultados$AlternativaOptima = Alt_Laplace; #nombre de la alternativa óptima

    return(resultados); #devolvemos los resultados

}

## Punto Ideal

criterio.PuntoIdeal = function(tablaX,favorable=TRUE) {

    X = tablaX;
    if (favorable) {
        MejoresPT = apply(X,MARGIN=2,max); # favorable
        AltPT = rep(0,dim(X)[1])
        for (i in 1:dim(X)[1]) {
            AltPT[i] = distanciaEuclidea(MejoresPT,X[i,])
        }
        ##AltPT
        names(AltPT) = rownames(tablaX)
        PuntoIdeal = min(AltPT);
        Alt_PuntoIdeal = which.min.general(AltPT);
        metodo = 'favorable';
    } else {
        MejoresPT = apply(X,MARGIN=2,min); # desfavorable
        AltPT = rep(0,dim(X)[1])
        names(AltPT) = rownames(tablaX)
        for (i in 1:dim(X)[1]) {
            AltPT[i] = distanciaEuclidea(MejoresPT,X[i,])
        }
        ##AltPT
        PuntoIdeal = min(AltPT);
        Alt_PuntoIdeal = which.min.general(AltPT);
        metodo = 'desfavorable';
    }
    resultados = list();
    resultados$criterio = 'Punto Ideal';
    resultados$metodo = metodo;
    resultados$tablaX = tablaX;
    resultados$Mejores = MejoresPT;
    resultados$ValorAlternativas = AltPT;
    resultados$ValorOptimo = PuntoIdeal;
    resultados$AlternativaOptima = Alt_PuntoIdeal;

    return(resultados);

}

## Todos los criterios

criterio.Todos = function(tablaX,alfa=0.3,favorable=TRUE) {

    cri01 = criterio.Wald(tablaX,favorable);
    cri02 = criterio.Optimista(tablaX,favorable);
    cri03 = criterio.Hurwicz(tablaX,alfa,favorable);
    cri04 = criterio.Savage(tablaX,favorable);
    cri05 = criterio.Laplace(tablaX,favorable);
    cri06 = criterio.PuntoIdeal(tablaX,favorable);

    numestados = ncol(tablaX)
    numalterna = nrow(tablaX)

    resultado = cbind(tablaX,cri01$ValorAlternativas,cri02$ValorAlternativas,
                      cri03$ValorAlternativas,cri04$ValorAlternativas,
                      cri05$ValorAlternativas,cri06$ValorAlternativas);

    decopt = c(rep(NA,numestados),cri01$AlternativaOptima[1],
               cri02$AlternativaOptima[1],cri03$AlternativaOptima[1],
               cri04$AlternativaOptima[1],cri05$AlternativaOptima[1],
               cri06$AlternativaOptima[1]);

    resultado = rbind(resultado,decopt);

    colnames(resultado)[numestados+1] = cri01$criterio;
    colnames(resultado)[numestados+2] = cri02$criterio;
    colnames(resultado)[numestados+3] = cri03$criterio;
    colnames(resultado)[numestados+4] = cri04$criterio;
    colnames(resultado)[numestados+5] = cri05$criterio;
    colnames(resultado)[numestados+6] = cri06$criterio;

    if (favorable) {
        rownames(resultado)[numalterna+1] = 'iAlt.Opt (fav.)';
    } else {
        rownames(resultado)[numalterna+1] = 'iAlt.Opt (Desfav.)';
    }


    resultado = as.data.frame(resultado)
    resultado[,"Veces Optimo"]=0 #Añadimos una columna (inicialmente toda de ceros) que introduzca cuantas veces es esa alternativa óptima

    resultado = format(resultado,digits=4)

    decopt = c(rep('--',numestados),
               paste0(names(cri01$AlternativaOptima),collapse = ","),
               paste0(names(cri02$AlternativaOptima),collapse = ","),
               paste0(names(cri03$AlternativaOptima),collapse = ","),
               paste0(names(cri04$AlternativaOptima),collapse = ","),
               paste0(names(cri05$AlternativaOptima),collapse = ","),
               paste0(names(cri06$AlternativaOptima),collapse = ","),
               paste0("-",collapse = ",")); #inicialmente es un guión

    resultado[nrow(resultado),] = decopt

    vectorconteo=vector()#vector inicialmente vacío

    #hacemos bucles anidados que recorran la lista resultante de separar el vector de
    #alternativas óptimas que estén entre comas (esto ocurre en los casos de empate)
    for(i in 1:length((strsplit(decopt, split=",")[-c(1:numestados,numestados+6+1)])))
    {
        for(j in 1:length((strsplit(decopt, split=",")[-c(1:numestados,numestados+6+1)])[[i]]))

            vectorconteo=cbind(vectorconteo,strsplit(decopt, split=",")[-c(1:numestados,numestados+6+1)][[i]][j])#para cada elemento de dicha lista se concatena con el vector inicialmente vacío
    }

    conteo=table(vectorconteo);#calculamos las frecuencias absolutas (conteo) asociado a
    #el vector formado por todos los nombres de alternativas que han resultado óptimas

    for(i in 1:nrow(conteo)){
        resultado[rownames(conteo)[i],"Veces Optimo"]=conteo[i];
    }#introducimos los valores en la última columna de la tabla de resultados

    contopt=which.max.general(conteo);
    mejoralt=contopt;

    resultado[numalterna+1,"Veces Optimo"]=paste0(names(mejoralt),collapse = ",")#añadimos la alternativa óptima (la que tiene un mayor conteo) al pie de la columna (en caso de empate, estas alternativas se mostrarán unidas separadas por coma)


    #usando operadores tubería para hacer un anidamiento de estilo, damos cabecera a la
    #tabla y se modifica la fuente de la letra y el marco de la misma
    # además hacemos que al pasar el ratón se sombree la fila de color amarillo
    # y que la tabla sea responsiva (varíe su tamaño en función del tamaño de ventana)
    # esta tabla resultante nos aparecerá al ejecutar la función en el "Viewer" de RStudio
   return(resultado %>%
               kbl(caption = "Criterios Decisión Bajo Incertidumbre") %>%
               kable_classic(full_width = F, html_font = "Cambria") %>%
               kable_paper("hover", full_width = F)%>%
               kable_styling(
                   bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                   full_width = F
               ) %>%
               column_spec(numestados+6+2,color = "white",
                   background = ifelse(resultado[,"Veces Optimo"] >=max(conteo), "#3CB371", "#FA8072"))%>%
               row_spec(0, bold = TRUE)

           )

}

