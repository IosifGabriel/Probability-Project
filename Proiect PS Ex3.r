
# Functia care genereaza repartitia comuna incompleta

frepcomgen = function(m, n) {
  m=2
  n=3
  
  # Generam m valori pentru x in intervalul [0, m * 10] si
  # n valori pentru y in [0, n * 10]
  xValues <- sample.int(m * 10, m);
  yValues <- sample.int(n * 10, n);
  
  # Adaugam in acesti vectori Qi si Pi pentru capetele de tabel
  xValues <- c(xValues, 'Qi')
  yValues <- c(yValues, 'Pi')
  
  # Construim tabelul de repartitie, reprezentat ca o matrice, unde valoarea -1
  # semnifica faptul ca probabilitatea pentru (X = xi si Y=yi) nu este cunoscuta
  repart <- matrix(-1, nrow=m+1, ncol=n+1)
  rownames(repart) <- xValues
  colnames(repart) <- yValues
  repart[m+1, n+1] = 1
  
  
  # Completam repartitia cu valori aleatoare, dar care vor respecta totusi conditia ca
  # suma pe linii/coloane sa fie mai mica ca 1 (sau egala)
  repart[1, 4] <- runif(1, 0, 1)
  repart[1, 1] <- runif(1, 0, repart[1, 4])
  
  repart[3, 1] <- runif(1, repart[1, 1], 1)
  
  repart[2, 2] <- runif(1, 0, 1 - repart[3, 1])
  while(repart[3, 2] - repart[2, 2] + repart[1, 1] > repart[1, 4] || 
          repart[3, 1] - repart[1, 1] + repart[2, 2] > 1 - repart[1, 4])
    repart[2, 2] <- runif(1, 0, 1 - repart[3, 1])
    
  repart[3, 2] <- runif(1, min(repart[2, 2], repart[3, 1]), 1 - max(repart[2, 2], repart[3, 1]))
  while(repart[3, 2] + repart[3, 1] > 1 || repart[2, 2] > repart[3, 2])
    repart[3, 2] <- runif(1, min(repart[2, 2], repart[3, 1]), 1 - max(repart[2, 2], repart[3, 1]))
  
  return(repart)
}

# Functia fcomplrepcom efectueaza operatii pe linii si coloane pentru a determina valorile
# necunoscute din repartitia comuna
fcomplrepcom <- function(repart) {
  repart[2, 4] = 1 - repart[1, 4]
  repart[1, 2] = repart[3, 2] - repart[2, 2]
  repart[1, 3] = repart[1, 4] - repart[1, 1] - repart[1, 2]
  repart[2, 1] = repart[3, 1] - repart[1, 1]
  repart[2, 3] = repart[2, 4] - repart[2, 2] - repart[2, 1]
  repart[3, 3] = 1 - repart[3, 1] - repart[3, 2]
  
  data.frame(repart)
}

repart <- frepcomgen(2, 3)
#repart
fcomplrepcom(repart)

