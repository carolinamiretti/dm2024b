#!/usr/bin/env Rscript

# Experimentos Colaborativos Default
# Workflow  Data Drifting repair

# limpio la memoria
rm(list = ls(all.names = TRUE)) # remove all objects
gc(full = TRUE) # garbage collection

require("data.table")
require("yaml")

#cargo la libreria
# args <- c( "~/dm2024a" )
args <- commandArgs(trailingOnly=TRUE)
source( paste0( args[1] , "/src/lib/action_lib.r" ) )

#------------------------------------------------------------------------------
# deflaciona por IPC
# momento 1.0  31-dic-2020 a las 23:59

drift_deflacion <- function(campos_monetarios) {
  cat( "inicio drift_deflacion()\n")
  vfoto_mes <- c(
    201901, 201902, 201903, 201904, 201905, 201906,
    201907, 201908, 201909, 201910, 201911, 201912,
    202001, 202002, 202003, 202004, 202005, 202006,
    202007, 202008, 202009, 202010, 202011, 202012,
    202101, 202102, 202103, 202104, 202105, 202106,
    202107, 202108, 202109
  )
  
  vIPC <- c(
    1.9903030878, 1.9174403544, 1.8296186587,
    1.7728862972, 1.7212488323, 1.6776304408,
    1.6431248196, 1.5814483345, 1.4947526791,
    1.4484037589, 1.3913580777, 1.3404220402,
    1.3154288912, 1.2921698342, 1.2472681797,
    1.2300475145, 1.2118694724, 1.1881073259,
    1.1693969743, 1.1375456949, 1.1065619600,
    1.0681100000, 1.0370000000, 1.0000000000,
    0.9680542110, 0.9344152616, 0.8882274350,
    0.8532444140, 0.8251880213, 0.8003763543,
    0.7763107219, 0.7566381305, 0.7289384687
  )
  
  tb_IPC <- as.data.table( list( vfoto_mes, vIPC) )
  
  colnames( tb_IPC ) <- c( envg$PARAM$dataset_metadata$periodo, "IPC" )
  
  dataset[tb_IPC,
          on = c(envg$PARAM$dataset_metadata$periodo),
          (campos_monetarios) := .SD * i.IPC,
          .SDcols = campos_monetarios
  ]
  
  cat( "fin drift_deflacion()\n")
}


#------------------------------------------------------------------------------
# deflaciona por dolar oficial
# momento 1.0  31-dic-2020 a las 23:59

drift_ajustedolar <- function(campos_monetarios) {
  cat( "inicio drift_ajustedolar()\n")
  vfoto_mes <- c(
    201901, 201902, 201903, 201904, 201905, 201906,
    201907, 201908, 201909, 201910, 201911, 201912,
    202001, 202002, 202003, 202004, 202005, 202006,
    202007, 202008, 202009, 202010, 202011, 202012,
    202101, 202102, 202103, 202104, 202105, 202106,
    202107, 202108, 202109
  )
  
  vdolar <- c(
    2.2870879121, 2.1736292428, 1.9680851064, 
    1.9270833333, 1.9006849315, 2.0060240964, 
    1.9405594406, 1.4605263158, 1.5000000000, 
    1.4299209894, 1.4415584416, 1.4353448276, 
    1.4353448276, 1.4110169492, 1.3703703704, 
    1.3058823529, 1.2709923664, 1.2153284672, 
    1.1684210526, 1.1404109589, 1.1100000000, 
    1.0741935484, 1.0341614907, 1.0000000000, 
    0.9652173913, 0.9380281690, 0.9098360656, 
    0.9000000000, 0.8880000000, 0.8786279683, 
    0.8694516971, 0.8604651163, 0.8494897959
    
  )
  
  tb_dolar <- as.data.table( list( vfoto_mes, vdolar) )
  
  colnames( tb_dolar ) <- c( envg$PARAM$dataset_metadata$periodo, "dolar" )
  
  dataset[tb_dolar,
          on = c(envg$PARAM$dataset_metadata$periodo),
          (campos_monetarios) := .SD * i.dolar,
          .SDcols = campos_monetarios
  ]
  
  cat( "fin drift_ajustedolar()\n")
}

#------------------------------------------------------------------------------
# deflaciona por CER
# momento 1.0  31-dic-2020 a las 23:59

drift_deflacionCER <- function(campos_monetarios) {
  cat( "inicio drift_deflacionCER()\n")
  vfoto_mes <- c(
    201901, 201902, 201903, 201904, 201905, 201906,
    201907, 201908, 201909, 201910, 201911, 201912,
    202001, 202002, 202003, 202004, 202005, 202006,
    202007, 202008, 202009, 202010, 202011, 202012,
    202101, 202102, 202103, 202104, 202105, 202106,
    202107, 202108, 202109
  )
  
  vCER <- c(
    2.0081683773, 1.9572307055, 1.8907355493, 
    1.8147605048, 1.7432238390, 1.6892592102, 
    1.6409505481, 1.6018434743, 1.5542840769, 
    1.4796631418, 1.4160117305, 1.3632659391, 
    1.3109563894, 1.2740524927, 1.2463481169, 
    1.2145868767, 1.1858806784, 1.1686377513, 
    1.1470123770, 1.1240278292, 1.0990959531, 
    1.0692034122, 1.0355201911, 1.0000000000, 
    0.9651374966, 0.9299058944, 0.8941007719, 
    0.8585644960, 0.8214587213, 0.7926691250, 
    0.7673274181, 0.7442808931, 0.7247084173 
    
  )
  
  tb_CER <- as.data.table( list( vfoto_mes, vCER) )
  
  colnames( tb_CER ) <- c( envg$PARAM$dataset_metadata$periodo, "CER" )
  
  dataset[tb_CER,
          on = c(envg$PARAM$dataset_metadata$periodo),
          (campos_monetarios) := .SD * i.CER,
          .SDcols = campos_monetarios
  ]
  
  cat( "fin drift_deflacionCER()\n")
}
#------------------------------------------------------------------------------

drift_rank_simple <- function(campos_drift) {
  
  cat( "inicio drift_rank_simple()\n")
  for (campo in campos_drift)
  {
    cat(campo, " ")
    dataset[, paste0(campo, "_rank") :=
              (frank(get(campo), ties.method = "random") - 1) / (.N - 1), by = eval(envg$PARAM$dataset_metadata$periodo)]
    dataset[, (campo) := NULL]
  }
  cat( "fin drift_rank_simple()\n")
}
#------------------------------------------------------------------------------
# El cero se transforma en cero
# los positivos se rankean por su lado
# los negativos se rankean por su lado

drift_rank_cero_fijo <- function(campos_drift) {
  
  cat( "inicio drift_rank_cero_fijo()\n")
  for (campo in campos_drift)
  {
    cat(campo, " ")
    dataset[get(campo) == 0, paste0(campo, "_rank") := 0]
    dataset[get(campo) > 0, paste0(campo, "_rank") :=
              frank(get(campo), ties.method = "random") / .N, by = eval(envg$PARAM$dataset_metadata$periodo)]
    
    dataset[get(campo) < 0, paste0(campo, "_rank") :=
              -frank(-get(campo), ties.method = "random") / .N, by = eval(envg$PARAM$dataset_metadata$periodo)]
    dataset[, (campo) := NULL]
  }
  cat("\n")
  cat( "fin drift_rank_cero_fijo()\n")
}
#------------------------------------------------------------------------------

drift_estandarizar <- function(campos_drift) {
  
  cat( "inicio drift_estandarizar()\n")
  for (campo in campos_drift)
  {
    cat(campo, " ")
    dataset[, paste0(campo, "_normal") := 
              (get(campo) -mean(campo, na.rm=TRUE)) / sd(get(campo), na.rm=TRUE),
            by = eval(envg$PARAM$dataset_metadata$periodo)]
    
    dataset[, (campo) := NULL]
  }
  cat( "fin drift_estandarizar()\n")
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui comienza el programa
cat( "z541_DR_corregir_drifting.r  START\n")
action_inicializar() 

# cargo el dataset donde voy a entrenar
# esta en la carpeta del exp_input y siempre se llama  dataset.csv.gz
# cargo el dataset
envg$PARAM$dataset <- paste0( "./", envg$PARAM$input, "/dataset.csv.gz" )
envg$PARAM$dataset_metadata <- read_yaml( paste0( "./", envg$PARAM$input, "/dataset_metadata.yml" ) )

cat( "lectura del dataset\n")
action_verificar_archivo( envg$PARAM$dataset )
cat( "Iniciando lectura del dataset\n" )
dataset <- fread(envg$PARAM$dataset)
cat( "Finalizada lectura del dataset\n" )

GrabarOutput()

# ordeno dataset
setorderv(dataset, envg$PARAM$dataset_metadata$primarykey)

# por como armé los nombres de campos,
#  estos son los campos que expresan variables monetarias
campos_monetarios <- colnames(dataset)
campos_monetarios <- campos_monetarios[campos_monetarios %like%
                                         "^(m|Visa_m|Master_m|vm_m)"]

# aqui aplico un metodo para atacar el data drifting
# hay que probar experimentalmente cual funciona mejor
switch(envg$PARAM$metodo,
       "ninguno"        = cat("No hay correccion del data drifting"),
       "rank_simple"    = drift_rank_simple(campos_monetarios),
       "rank_cero_fijo" = drift_rank_cero_fijo(campos_monetarios),
       "deflacion"      = drift_deflacion(campos_monetarios),
       "estandarizar"   = drift_estandarizar(campos_monetarios),
       "dolar_oficial"  = drift_ajustedolar(campos_monetarios),
       "deflacionCER"   = drift_deflacionCER(campos_monetarios)
)


#------------------------------------------------------------------------------
# grabo el dataset
cat( "escritura del dataset\n")
cat( "Iniciando grabado del dataset\n" )
fwrite(dataset,
       file = "dataset.csv.gz",
       logical01 = TRUE,
       sep = ","
)
cat( "Finalizado grabado del dataset\n" )

# copia la metadata sin modificar
cat( "escritura de metadata\n")
write_yaml( envg$PARAM$dataset_metadata, 
            file="dataset_metadata.yml" )

#------------------------------------------------------------------------------

# guardo los campos que tiene el dataset
tb_campos <- as.data.table(list(
  "pos" = 1:ncol(dataset),
  "campo" = names(sapply(dataset, class)),
  "tipo" = sapply(dataset, class),
  "nulos" = sapply(dataset, function(x) {
    sum(is.na(x))
  }),
  "ceros" = sapply(dataset, function(x) {
    sum(x == 0, na.rm = TRUE)
  })
))

fwrite(tb_campos,
       file = "dataset.campos.txt",
       sep = "\t"
)

#------------------------------------------------------------------------------
cat( "Fin del programa\n")

envg$OUTPUT$dataset$ncol <- ncol(dataset)
envg$OUTPUT$dataset$nrow <- nrow(dataset)
envg$OUTPUT$time$end <- format(Sys.time(), "%Y%m%d %H%M%S")
GrabarOutput()

#------------------------------------------------------------------------------
# finalizo la corrida
#  archivos tiene a los files que debo verificar existen para no abortar

action_finalizar( archivos = c("dataset.csv.gz","dataset_metadata.yml")) 
cat( "z541_DR_corregir_drifting.r  END\n")
