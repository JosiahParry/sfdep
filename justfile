default:
    just --list

fmt:
    air format R/* tests/* && jarl check R/* tests/testthat/* --fix --allow-dirty

lint:
    jarl check R/* tests/testthat/* && air format --check R/* tests/*

test:
    R -q -e "devtools::test()"

readme:
    R -q -e "devtools::build_readme()"
