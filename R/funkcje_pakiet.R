
#' @title Przyrosty
#' @author Karolina Dudzinska, Bartosz Nguyen Van
#' @description Funkcja zwraca przyrosty dla danych: standardowe lub logarytmiczne.
#' @param x macierz lub numeryczny wektor danych zwierajacy wartosci do zroznicowania
#' @param lag wartosc numeryczna typu integer okreslajaca liczne opoznien
#' @param roznice  wartosc numeryczna typu integer okreslajaca kolejnosc roznicowania, ile razy roznicowanie ma byc przeprowadzone na danych
#' @param logarytm  wartosc typu bool (TRUE lub FALSE), okreslajaca czy dane maja zostac zlogarytmizowane, defaultowo dane nie sa przeksztalcane na logarytm
#' @param ... inne argumenty
#' @return Jesli x jest wektorem o dlugosci n i roznica = 1, to obliczony wynik jest rowny kolejnym roznicom x[(1+lag):n] - x[1:(n-lag)].
#' Jezeli roznica jest wieksza od 1, to algorytm ten jest stosowany rekurencyjnie do x.
#' Zwracana wartoscia jest wektor krotszy od x.
#' Jezeli x jest macierza, to operacje roznicy sa wykonywane na kazdej kolumnie osobno.
#' @rdname przyrosty
#' @export
przyrosty <- function(x, lag, roznice, logarytm, ...) UseMethod("przyrosty")


#' @title Przyrosty
#' @author Karolina Dudzinska, Bartosz Nguyen Van
#' @description Funkcja zwraca przyrosty dla danych: standardowe lub logarytmiczne.
#' @param x macierz lub numeryczny wektor danych zwierajacy wartosci do zroznicowania
#' @param lag wartosc numeryczna typu integer okreslajaca liczne opoznien
#' @param roznice  wartosc numeryczna typu integer okreslajaca kolejnosc roznicowania, ile razy roznicowanie ma byc przeprowadzone na danych
#' @param logarytm  wartosc typu bool (TRUE lub FALSE), okreslajaca czy dane maja zostac zlogarytmizowane, defaultowo dane nie sa przeksztalcane na logarytm
#' @param ... inne argumenty
#' @return Jesli x jest wektorem o dlugosci n i roznica = 1, to obliczony wynik jest rowny kolejnym roznicom x[(1+lag):n] - x[1:(n-lag)].
#' Jezeli roznica jest wieksza od 1, to algorytm ten jest stosowany rekurencyjnie do x.
#' Zwracana wartoscia jest wektor krotszy od x.
#' Jezeli x jest macierza, to operacje roznicy sa wykonywane na kazdej kolumnie osobno.
#' @rdname przyrosty.default
#' @export
przyrosty.default <- function (x, lag = 1L, roznice = 1L, logarytm = FALSE, ...)
{
  stopifnot(is.numeric(x))

  if (logarytm)
    x <- log(x)
  ismat <- is.matrix(x)
  xlen <- if (ismat)
    dim(x)[1L]
  else length(x)
  if (length(lag) != 1L || length(roznice) > 1L || lag <
      1L || roznice < 1L)
    stop("'lag' oraz 'roznice' musza byc typu integer oraz wieksze lub rowne 1")
  if (lag * roznice >= xlen)
    return(x[0L])

  r <- unclass(x)
  i1 <- -seq_len(lag)

  if (ismat)
    for (i in seq_len(roznice)) r <- r[i1, , drop = FALSE] -
    r[-nrow(r):-(nrow(r) - lag + 1L), , drop = FALSE]
  else for (i in seq_len(roznice)) r <- r[i1] - r[-length(r):-(length(r) - lag + 1L)]
  class(r) <-oldClass(r)
  class(r)<- append(class(r),'przyrosty')
  return(r)
}


#' @title Przyrosty Print
#' @author Karolina Dudzinska, Bartosz Nguyen Van
#' @description Funkcja wypisuje obliczone wczesniej przyrosty dla danych: standardowe lub logarytmiczne.
#' @param x wynik roznicowania
#' @param ... inne argumenty
#' @rdname print.przyrosty
#' @export
print.przyrosty <- function(x, ...){

  x <- unclass(x)
  class(x) <- "numeric"
  print(x)
}

#' @title Przyrosty Plot
#' @author Karolina Dudzinska, Bartosz Nguyen Van
#' @description Funkcja zwraca wykres dla obliczonych wczesniej przyrostow dla danych.
#' @param x wynik roznicowania
#' @param ... inne argumenty
#' @rdname plot.przyrosty
#' @export
plot.przyrosty <- function(x, ...){
  x <- unclass(x)
  class(x) <- "numeric"
  plot(x, type="b", col ="plum4", main ="Przyrosty", xlab = "indeks")
}


#' @title Przyrosty Summary
#' @author Karolina Dudzinska, Bartosz Nguyen Van
#' @description Funkcja zwraca podstawowe statystyki opisowe dla obliczonych wczesniej przyrostow.
#' @param object wynik roznicowania
#' @param ... inne argumenty
#' @rdname summary.przyrosty
#' @export
summary.przyrosty <- function(object, ...){
  cat("Podstawowe statystyki opisowe:\n")
  object <- unclass(object)
  class(object) <- "numeric"
  summary(object)
}





