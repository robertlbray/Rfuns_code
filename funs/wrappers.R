
setLevels <- function(x, old.levels = x, new.levels = old.levels){
  #This function changes both the names and orders of a factor's levels. Order old.levels and new.levels in the new order you want
  x = factor(x, levels = old.levels)
  levels(x) <- new.levels
  x
}

factor_to_int <- function(x) {
  as.integer(levels(x))[x]
}

df <- function(...) data_frame(...)

dm <- function(...) dim(...)

un <- function(...) unlist(...)

ad <- function(...) as.data.frame(...)

rd <- function(...) reduce(...)

le <- function(file.name, loc = varSave) readRDS(paste0(loc, file.name))

se <- function(d, file.name, loc = varSave) saveRDS(d, paste0(loc, file.name), compress = FALSE) 

gb <- function(...) group_by(...)

mt <- function(...) mutate(...)

tm <- function(...) transmute(...)

me <- function(...) mutate_each(...)

sl <- function(...) dplyr::select(...)

sm <- function(...) summarise(...)

Sm <-  function(...) summary(...)

fl <- function(...) dplyr::filter(...)

ag <- function(...) arrange(...)

rn <- function(...) rename(...)

ug <- function(...) ungroup(...)

ct <- function(...) dplyr::count(...)

ds <- function(...) distinct(...)

dd <- function(...) ddply(...)

dl <- function(...) dlply(...)

ld <- function(...) ldply(...)

ll <- function(...) llply(...)

d_ <- function(...) d_ply(...)

l_ <- function(...) l_ply(...)

dc <- function(...) dcast(...)

ml <- function(...) melt(..., factorsAsStrings = FALSE)

aj <- function(...) anti_join(...)

ij <- function(...) inner_join(...)

fj <- function(...) full_join(...)

lj <- function(...) left_join(...)

aj <- function(...) anti_join(...)

cj <- function(x1, x2) { #cartesian join
  fj(
    mt(x1, ..merge.all.. = TRUE),
    mt(x2, ..merge.all.. = TRUE)
  ) %>%
    sl(-..merge.all..)
}

p0 <- function(...) paste0(...)

h <- function(...) head(...)

tl <- function(...) tail(...)

sn <- function(d, size = min(10, nrow(d)), replace = FALSE) sample_n(d, size, replace)

V <- function(...) View(...)

