
## proPNLD 0.1.4 (2020-08-01 - github release)

* mudança de nome do pacote:
  + de projPNLD para `proPNLD`

* funções:
  + `insert_table_into_db()`:
    + utilização de `tryCatch()` para que seja possíveldar um "commit" ainda que 
    em caso de erros de inserção

---

## projPNLD 0.1.3 (2020-07-30 - local)

* bugs corrigidos:
  + `extract_and_clean_censo_full()`:
    + incluido `CO_MUNICIPIO` no argumento `by` do join devido aos casos
    um mesmo `CO_ENTIDADE` em mais de um município. Isso resultava em fitting e
    forecasting para turmas/escolas desnecessárias pois não possuíam entrada no
    último censo, além de trazer `NA`s nos nomes de munícipio, entidade e etapa,
    para a tabela técnica final em combinação com o `full_join()` usado 
    anteriormente em `make_final_table()`;
  + `make_final_table()`:
    + alterado de `full_join()` para `left_join()` para trazer apenas os dados de
    de nomes conforme último censo - contribuía para trazer a tabela técnica cheia
    de `NA`s.
    + alguns nomes de colunas alterados para atender aos requisitos do pessoal
    de banco de dados
    + transformação dos `NaN`s da coluna `MASE` - isso aconteceia para séries de
    tamanho 1, uma vez que não era possível calcular o erro. Como consequência,
    era atribuído `NaN` para a entrada e isso causava um erro na hora de escrever 
    no banco.
  + `make_final_table()`:
    + nomes ajustados conforme saída da `make_final_table`
    
* novas funções:
  + `insert_table_into_db()`:
    + função que grava os dados no banco de dados


---

## projPNLD 0.1.2 (2020-07-09 - github release)

* pacote funcionando no conatiner docker

* arquivos de output gravados em `CSV`


---

## projPNLD 0.1.2 (2020-07-02 - internal)

* segunda versão de testes

## projPNLD 0.1.2 (2020-06-30 - internal)

* segunda versão de testes
