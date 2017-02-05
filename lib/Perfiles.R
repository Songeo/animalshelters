Perfiles <-
  function(contingencia, tipo='d'){ #d perfiles dobles, r perfiles renglon, c perfiles columna
    
    if(tipo %in% c('c','d','r'))
    {
      
      if (tipo=='d')
      {
        atr_promedio <- prop.table(margin.table(contingencia, margin = 1))
        tabla_prop <- prop.table(contingencia, margin = 2)
        perfiles <- t(scale(t(tabla_prop), center = FALSE, 
                            scale = atr_promedio))
        #write.table(round(100*perfiles_col), sep = ",")
        res <- round(100*perfiles)
      }
      else
      {
        
        if(tipo=='r')
        {
          n <- ncol(contingencia)
          cont.2 <- t(contingencia)
          margen=1
        }
        else
        {
          n <- nrow(contingencia)
          cont.2 <- contingencia
          margen=2
        }
        atr_promedio <- (margin.table(contingencia, margin = margen)/n)
        
        perfiles <- (scale(cont.2,center = FALSE, 
                           scale = atr_promedio))
        #write.table(round(100*perfiles_col), sep = ",")
        res <- round(100*perfiles)
        if(tipo=='r')
        {
          res=t(res)
        }
      }
      
    }
    else
    {
      print("ERROR. El tipo debe ser c,d ó r.")
      res="ERROR"
    }
    res
  }


perfilesColumna <-
  function(contingencia){
    atr_promedio <- (margin.table(contingencia, margin = 2)/nrow(contingencia))
    perfiles_col <- (scale(contingencia,center = FALSE, 
                           scale = atr_promedio))
    #write.table(round(100*perfiles_col), sep = ",")
    round(100*perfiles_col)
  }

perfilesRenglon <-
  function(contingencia){
    atr_promedio <- (margin.table(contingencia, margin = 1)/ncol(contingencia))
    perfiles_reng <- (scale(t(contingencia),center = FALSE, 
                            scale = atr_promedio))
    #write.table(round(100*perfiles_col), sep = ",")
    t(round(100*perfiles_reng))
  }

perfiles_boot <- function(datos, header, var, by, n.boot = 100, marcar.tol = 20){
  # Calcular primero puntuales
  dat <- datos[, c(header, var, by)]
  tablas <- function(datos, header, var, by){
    tab.raw <- ddply(datos, c(header, by) , function(df){
      salida <- data.frame(table(df[, var]))
      names(salida) <- c( var, 'Freq')
      salida$Base <- sum(salida$Freq)
      salida
    })
    tab.prop <- ddply(tab.raw, c(header, by), transform, Prop = 100*Freq/sum(Freq))
    tab.momios <- ddply(tab.prop, var, transform, Perfil = 100*Prop/mean(Prop))
    tab.momios
  }
  
  muestra.boot <- function(datos, header, by){
    datos.boot <- ddply(datos, c(header, by), function(df){
      muestra.reng <- sample(1:nrow(df), nrow(df), replace = TRUE)
      df[muestra.reng, ]   
    })
    datos.boot
  }
  reps <- rdply(n.boot, function(){
    tablas(muestra.boot(dat, header, by), header, var, by)
  })
  out <- ddply(reps, c(header,var, by), summarise, 
               Base = mean(Base),
               Freq = mean(Freq),
               Prop = mean(Prop),
               Perfil = mean(Perfil),
               Perfil.sd = sd(Perfil),
               Perfil.10 = quantile(Perfil, 0.10, na.rm = TRUE),
               Perfil.90 = quantile(Perfil, 0.90, na.rm = TRUE)
  )
  out$marcar <- (out$Perfil.90-100)*(out$Perfil.10-100) > 0
  out$marcar.dif <- out$marcar & (out$Perfil > 100+marcar.tol | out$Perfil < 100-marcar.tol)
  out
}