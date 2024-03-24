# PACOTES ----
if (!require(pacman)){
  install.packages("pacman")} else{
    library(pacman)}


pacman::p_load(gitcreds, usethis)

# Tutorial: https://www.youtube.com/watch?v=2gmofUthjKk

# Problema de conflito de teclas para quem tem pci de video AMD
# https://www.youtube.com/watch?v=Q0OrayIbUnQ

# Se apresentar para o Git:
usethis::use_git_config(user.name = "Fernando Bispo",
                        user.email = "fobispo@outlook.com")

# Cria automaticamente um Token na conta o GitHub
usethis::create_github_token()

usethis::edit_r_environ()

gitcreds::gitcreds_set()
usethis::use_git()
usethis::use_github()


usethis::git_sitrep()



# K*\L\c5zmfb
# bd23ec8598c56ac17f6721e172e3821777d8d5f2


# arthur.rios@ufba.br













