# PACOTES ----
if (!require(pacman)){
  install.packages("pacman")} else{
    library(pacman)}


pacman::p_load(gitcreds, usethis)

# Tutorial: https://www.youtube.com/watch?v=2gmofUthjKk


# https://curso-r.githud.io/zen-do-r/git-githud.html
# Se apresentar para o Git:
usethis::use_git_config(user.name = "Fernando Bispo",
                        user.email = "fobispo@outlook.com")

# Cria automaticamente um Tokn na conta o GitHub
usethis::create_github_token()


usethis::edit_r_environ()












gitcreds::gitcreds_set()
usethis::use_git()
usethis::use_github()