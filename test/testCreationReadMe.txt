Nota: Enquanto não finalizamos os testes mantemos os ficheiros a mais nesta pasta
      Quando tivermos tudo acabado apagamos todos os ficheiros text*

Acho que não vais achar util, mas para escrever as cenas dos testes em haskell
é uma dor de cabeça, especialmente se tiveres a fazer debuging!! Se quiseres usar
tas a vontade, deixo te aqui como usar


-------Como Usar------
./test/testCreateScript.sh <---- Correr na pasta root (Não tive pachorra de fzr um makefile)

-------Como escrever-----

#Top of page
<Numero>       <--- Quantas proposições existem


start prop:    <--- Diz onde é que a proposição começa
<prop Name>

<prop body>
...
...
...
<prop body>

end prop:      <--- Diz onde é que acaba



start sol      <--- Diz onde começa a solução

<sol body>
...
...
...
<sol body

end sol:      <--- Diz onde acaba a solução
