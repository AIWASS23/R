# R Completo e Total

## 📐 Introdução

A linguagem R facilita a apresentação de seus dados com um estilo artístico e bonito. Algumas outras poucas vantagens de R incluem:

* Popularidade: R é usada frequentemente para análise de dados.

* Ferramentas: R possui uma biblioteca conveniente de ferramentas de preenchimento rápido para limpeza e análise de dados.

* Foco: R foi criada com a estatística em mente; os analistas de dados podem usar convenientemente uma rica biblioteca de rotinas estatísticas.

* Adaptabilidade: R se adapta bem para uso em projetos de machine learning e análise de dados.

* Disponibilidade: R é uma linguagem de programação de código aberto

## ® 🆚 🐍 O debate R versus Python

* **Este artigo é escrito por um profissional de dados com vasta experiência usando ambas as linguagens e fornece uma comparação detalhada.** - [R versus Python, a comprehensive guide for data professionals](https://medium.com/analytics-and-data/r-vs-python-a-comprehensive-guide-for-data-professionals-321e8dead598)
* **Este artigo fornece uma comparação das linguagens usando exemplos do uso de código.** - [R versus Python, an objective comparison](https://www.dataquest.io/blog/python-vs-r/)
* **Pespectiva do RStudio** - [blog](https://blog.rstudio.com/2019/12/17/r-vs-python-what-s-the-best-for-language-for-data-science/)

## 💻 Linguagens de programação 

Alguns cargos potenciais que podemos encontrar e as linguagens de programação mais populares usadas nessas profissões. Uma lista de recursos adicionais também está incluída para você explorar e aprender mais sobre cada linguagem de programação apresentada.

### Analista de dados

Um analista de dados coleta, transforma e organiza dados para tirar conclusões, fazer previsões e orientar um processo de tomada de decisão informado. As linguagens de programação mais populares usadas pelos analistas de dados são R e Python. 

#### R

Oferece funções estatísticas convenientes para a análise de dados e é útil para a criação de visualizações de dados avançadas. Verifique esses recursos para aprender mais sobre R

* **Site para fazer o download de R, documentação e ajuda** - [R](https://www.r-project.org/)
* **Links de manuais da equipe principal do R, incluindo introdução, administração e ajuda** - [Manuais](https://cran.r-project.org/manuals.html)
* **Coleção de tutoriais de codificação para o R** - [Tutoriais](https://ourcodingclub.github.io/tutorials.html)
* **Guia inicial para ajudar você a trabalhar com dados, gráficos e estatística em R** - [Guia](https://cran.r-project.org/doc/contrib/Paradis-rdebuts_en.pdf)

#### Python

* **Site com guias para ajudá-lo a começar como iniciante** - [Guia](https://www.python.org/about/gettingstarted/)
* **Tutorial Python 3 do site PSF** - [Tutoriais](https://docs.python.org/3/tutorial/)
* **Coleção de tutoriais de codificação para o R** - [Codificação em R](https://ourcodingclub.github.io/tutorials.html)

## 🔧 RStudio

Uma de suas principais tarefas como analista será converter dados brutos em insights que sejam precisos, úteis e interessantes. Isso pode ser complicado de fazer quando os dados brutos são complexos. O R e RStudio são projetados para lidar com grandes conjuntos de dados, os quais as planilhas podem não ser capazes de lidar também. O RStudio também facilita a reprodução do seu trabalho em diferentes bancos de dados. Quando você insere seu código, é simples carregar um novo conjunto de dados e executar seus scripts novamente. Você também pode criar visualizações mais detalhadas usando RStudio. 

* **Vantagens de usar RStudio para análise de dados** - [RStudio](https://www.theanalysisfactor.com/the-advantages-of-rstudio/)

## 🫱🏿‍🫲🏻 Comunidades

* **O fórum da comunidade RStudio** - [Forum](https://community.rstudio.com/)
* **O subreddit da linguagem R** - [Reddit](https://www.reddit.com/r/Rlanguage/)
* **O rOpenSci possui um fórum da comunidade onde os usuários de R podem fazer perguntas e buscar soluções.** - [rOpenSci](https://discuss.ropensci.org/)
* **Esta é uma comunidade com outro canal Slack onde alunos e mentores de R podem se reunir e se conectar.** - [Slack](https://www.rfordatasci.com/)
* **Se você usa o Twitter, pode se conectar com outros usuários de R usando a hashtag #rstats; muitos desenvolvedores e analistas de R estão ativos no Twitter.** - [Twitter](https://twitter.com/hashtag/rstats?lang=en)
* **Esses encontros são uma ótima maneira de conhecer outras pessoas interessadas em Data Analytics e desenvolver sua rede de contatos. Esses encontros são baseados em localidades, então você pode se conectar com outros analistas de dados em sua região.** - [meetup](https://www.meetup.com/topics/data-analytics/)
* **Esta lista contém links para comunidades regionais de R, incluindo subreddits e grupos de encontro. Este é um recurso útil se você estiver interessado em encontrar usuário de R em sua área.** - [Grupos](https://jumpingrivers.github.io/meetingsR/r-user-groups.html)
* **Estes são encontros presenciais e virtuais especificamente para entusiastas de R que se identificam como sub-representados ou marginalizados. Esses encontros também são baseados em localidades, então você pode se conectar com outros analistas de dados em sua região.** - [rLadies](https://www.meetup.com/pro/rladies)

## Vetores e Listas

### Vetores e listas em R

Em programação, estrutura de dados refere-se ao formato de organização e armazenamento de dados. É importante entendê-las, pois você irá trabalhar com essas estruturas com frequência ao usar R na análise de dados. As estruturas de dados mais comuns na linguagem de programação R incluem: 

* Vetores
* Data frames
* Matrizes
* Matrizes unidimensionais 

Os vetores dividem-se em dois tipos: vetores atômicos e listas. 

### Vetores atômicos 

Há seis tipos principais de vetores atômicos: logical (lógico), integer (números inteiros), double (de duplo vetor), character (que inclui strings), complex (números complexos) e raw (números não processados). Os dois últimos tipos (complex e raw) não são tão comuns na análise de dados, portanto, vamos nos concentrar nos quatro primeiros. Juntos, os vetores integer e double são conhecidos como vetores numéricos, pois ambos contêm números.

### Como criar vetores  

Uma forma de criar um vetor é usar a função c() (conhecida como função “combine”). A função c() em R combina vários valores em um vetor. Em R, esse função é apenas a letra “c” seguida pelos valores que você quer no vetor dentro dos parênteses, separados por uma vírgula: c(x, y, z, …).

Você pode usar, por exemplo, a função c() para armazenar dados numéricos em um vetor. 

c(2.5, 48.5, 101.5)

Para criar um vetor de números inteiros com a função c(), é necessário inserir a letra “L” logo após cada número.

c(1L, 5L, 15L)

Você também pode criar um vetor com caracteres ou lógicos. 

c(“Sara” , “Lisa” , “Anna”)
c(TRUE, FALSE, TRUE)

### Como definir as propriedades dos vetores 

Cada vetor criado terá duas principais propriedades: tipo e comprimento.  

Defina com qual tipo de vetor você trabalha com a função typeof(). Insira o código do vetor dentro dos parênteses da função. Ao executá-la, R informará o tipo. Por exemplo: 

typeof(c(“a” , “b”))

Use a função length() para determinar o comprimento de um vetor que já existe (ou seja, o número de elementos contidos nele). No exemplo, usamos um operador de atribuição para atribuir o vetor à variável x e, então, aplicamos a função length() à variável. Ao executarmos a função, R informa que o comprimento é de 3.

x <- c(33.5, 57.75, 120.05)
length(x)

Você também pode verificar se o vetor é de determinado tipo com a função is: is.logical(), is.double(), is.integer(), is.character(). No exemplo, R retorna um valor de TRUE pois o vetor contém números inteiros. 

x <- c(2L, 5L, 11L)
is.integer(x)

No exemplo, R retorna um valor de FALSE pois o vetor não inclui caracteres, mas sim elementos lógicos.

y <- c(TRUE, TRUE, FALSE)
is.character(y)

### Como nomear vetores 

Todos os tipos de vetores podem ser nomeados. Os nomes são úteis para se escrever um código legível e ao descrever objetos em R. Para nomear os elementos de um vetor, use a função names(). Vamos atribuir, por exemplo, a variável x a um novo vetor com três elementos. 

x <- c(1, 3, 5)

Use a função names() para atribuir um nome diferente para cada elemento do vetor. 

names(x) <- c("a", "b", "c")

Ao executar o código, R mostra que o primeiro elemento do vetor tem o nome de a, o segundo b e o terceiro c.

Lembre-se de que um vetor atômico só pode conter elementos do mesmo tipo. Para armazenar elementos de tipos diferentes na mesma estrutura de dados, opte pela lista. 

### Como criar listas

As listas diferem-se dos vetores atômicos pois seus elementos podem ser de qualquer tipo, como datas, data frames, vetores, matrizes, etc. Elas podem até mesmo conter outras listas. 

Use a função list() para criar uma lista. Assim como na função c(), a função list() é apenas list seguida dos valores que quer na lista dentro dos parênteses: list(x, y, z, …). No exemplo, criamos uma lista com quatro diferentes tipos de elementos: character ("a"), integer (1L), double (1.5) e logical (TRUE). 

list("a", 1L, 1.5, TRUE)

Como já dito antes, as listas podem conter outras listas. Você pode até mesmo armazenar uma lista dentro de uma lista dentro de uma lista (e por aí vai), se quiser. 

list(list(list(1 , 3, 5)))

### Como definir a estrutura das listas 

Use a função str() para saber os tipos de elementos inclusos em uma lista. Para isso, insira o código da lista dentro dos parênteses da função. Ao executar a função, R mostrará a estrutura de dados da lista com a descrição dos elementos e dos tipos destes.

Vamos aplicar a função str() em nosso primeiro exemplo de lista. 

str(list("a", 1L, 1.5, TRUE))

Nós executamos a função e, então, R nos informa que a lista contém quatro elementos, os quais consistem em quatro tipos diferentes: character (chr), integer (int), number (num) e logical (logi). 

Vamos usar a função str() para descobrir a estrutura do segundo exemplo.  Primeiramente, vamos atribuir a lista à variável z. Assim, fica mais fácil inserir na função str(). 

z <- list(list(list(1 , 3, 5)))

O recuo dos símbolos $ refletem a estrutura aninhada da lista. Veja que há três níveis (ou seja, há uma lista dentro de uma lista dentro de uma lista).  

### Como nomear listas

Assim como os vetores, as listas podem ser nomeadas. Você pode nomear os elementos de uma lista, com a função list(), ao criá-la pela primeira vez:

list('Chicago' = 1, 'New York' = 2, 'Los Angeles' = 3)

* - [Vertore e Listas](https://r4ds.had.co.nz/vectors.html#vectors)

## Datas e Horas

### Como carregar os pacotes tidyverse e lubridate

Antes de começar a trabalhar com datas e horas, é preciso carregar os pacotes tidyverse e lubridate. O lubridate é parte do pacote tidyverse.

Primeiramente, abra o RStudio. 

Ainda não instalou o tidyverse? Use a função install.packages() para instalar:

install.packages(“tidyverse”)
Em seguida, carregue os pacotes tidyverse e lubridate com a função library(). Primeiro, carregue o núcleo do tidyverse para disponibilizá-lo na atual sessão de R:

library(tidyverse)
Feito isso, carregue o pacote lubridate:

library(lubridate)
Agora você já está pronto para conhecer as ferramentas do pacote lubridate. 

### Como trabalhar com datas e horas 

Esta seção aborda os tipos de dados de datas e horas no R e como converter strings em formatos de data-hora.

Tipos

Há três tipos de dados em R que se referem a um instante de tempo:

Uma data ("2016-08-16")
Uma hora do dia (“20:11:59 UTC")
E uma data-hora, ou seja, data mais a hora ("2018-03-31 18:15:48 UTC")
O horário é estimado em UTC, que, em inglês, significa Tempo Coordenado Universal. Refere-se ao padrão principal que rege os relógios e horários do mundo.

Para obter, por exemplo, a data atual, execute a função today(). A data é exibida no formato ano, mês e dia. 

today()

Para obter a data-hora atual, execute a função now(). Observe que a hora é exibida no segundo mais próximo. 

now()

Possivelmente, você usará uma das três seguintes formas para criar formatos data-hora ao trabalhar com R: 

A partir de uma string
A partir de uma data específica
A partir de um objeto data/hora já existente
R cria datas no formato padrão aaaa-mm-dd.

### Como converter a partir de strings 

Em geral, dados de data/hora aparecem como strings. É possível converter strings em datas e data-hora com as ferramentas integradas ao pacote lubridate, que automaticamente executam o formato data/hora. Antes de tudo, identifique a ordem em que ano, mês e dia aparece em suas datas. Feito isso, ordene as letras y, m e d na mesma ordem para obter o nome da função lubridate que analisará sua data. Por exemplo, para a data 2021-01-20, use a ordem ymd:

ymd("2021-01-20")

O mesmo vale para qualquer ordem; por exemplo, mês, dia e ano. R ainda retorna a data no formato aaaa-mm-dd.

mdy("January 20th, 2021")

Ou dia, mês e ano. R ainda retorna a data no formato aaaa-mm-dd.

dmy("20-Jan-2021")

Essas funções também processam números sem aspas e os converte no formato aaaa-mm-dd.

ymd(20210120)

### Como criar componentes de data-hora

A função ymd() e suas variações criam datas. Para criar uma data-hora a partir de uma data, adicione um sublinhado e uma ou mais letras h, m e s (horas, minutos, segundos) ao nome da função:

ymd_hms("2021-01-20 20:11:59")
mdy_hm("01/20/2021 08:01")

Opcional: Como alternar entre objetos de data-hora que já existem 

Por fim, você pode querer alternar entre data-hora e data.

Use a função as_date() para converter uma data-hora em data. Insira, por exemplo, a atual date-time—now()—entre os parênteses da função. 

as_date(now())

* **Documentação oficial** - [Lubridates](https://lubridate.tidyverse.org/index.html)
* **Folha de referências com um mapa detalhado** - [Datas e Horas](https://rawgit.com/rstudio/cheatsheets/master/lubridate.pdf)

## DataFrames e Matriz

### DataFrames

Data frames são a forma mais comum de armazenar e analisar dados em R, por isso é importante saber o que são e como criá-los. O data frame nada mais é do que uma coleção de colunas semelhante a uma planilha ou tabela de SQL. Na parte superior de cada coluna, há um nome que representa uma variável; além disso, a coluna inclui uma observação por linha. Os data frames ajudam a resumir os dados e colocá-los em um formato que seja fácil de ler e usar. 

Ao trabalhar com data frames, é importante estar atento ao seguinte: 

Primeiro, as colunas devem ser nomeadas. 
Em segundo lugar, os data frames podem incluir diferentes tipos de dados, como numéricos, lógicos ou de caractere.
Por fim, os elementos da mesma coluna devem ser do mesmo tipo.

Use a função data.frame() para criar manualmente um data frame em R; essa função processa os vetores como input. Entre os parênteses, insira o nome da coluna, seguido de um sinal de igual, e, em seguida, o vetor que quer inserir para essa coluna. No exemplo, a coluna x é um vetor com os elementos 1, 2, 3, e a coluna y é um vetor com os elementos 1.5, 5.5, 7.5 

data.frame(x = c(1, 2, 3) , y = c(1.5, 5.5, 7.5))

Na maioria dos casos, não é preciso criar manualmente um data frame, pois você geralmente importa dados de outra fonte, como um arquivo .csv, um banco de dados relacional ou um programa de software.

#### Arquivos

Como criar, copiar e excluir arquivos em R. 
* - [Arquivos](https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/files)

Use a função dir.create para criar uma nova pasta, ou diretório, para guardar seus arquivos. Insira o nome da pasta entre os parênteses da função.

dir.create ("destination_folder")

Use a função file.create() para criar um arquivo em branco. Insira o nome e o tipo do arquivo entre os parênteses da função. Os tipos do seu arquivo serão, em geral, como .txt, .docx ou .csv.  

file.create (“new_text_file.txt”) 
file.create (“new_word_file.docx”) 
file.create (“new_csv_file.csv”) 

Se o arquivo for criado ao executar a função, R retornará um valor TRUE (caso contrário, retornará FALSE). 

file.create (“new_csv_file.csv”) 

É possível copiar um arquivo com a função file.copy(). Entre os parênteses, insira o nome do arquivo a ser copiado e, em seguida, adicione uma vírgula e o nome da pasta de destino na qual o arquivo será copiado. 

file.copy (“new_text_file.txt” , “destination_folder”)

Se você conferir o painel Arquivos (Files) em RStudio, verá uma cópia do arquivo na pasta relevante:

exclua arquivos de R com a função unlink(). Insira o nome do arquivo entre os parênteses da função.

unlink (“some_.file.csv”)

#### Recurso adicional
* - [Data Wrangling](http://statseducation.com/Introduction-to-R/modules/getting%20data/data-wrangling/)

### Matrizes 

Matriz é uma coleção bidimensional de elementos de dados, ou seja, tem linhas e colunas. Em contrapartida, um vetor é uma sequência unidimensional de elementos de dados. Mas, assim como os vetores, as matrizes contêm apenas um tipo de dado. Por exemplo, a matriz não pode incluir valores lógicos e numéricos juntos. 

Para criar uma matriz em R, use a função matrix(), que tem dois principais argumentos que você insere entre os parênteses. Primeiro, adicione um vetor, que contém os valores a serem colocados na matriz. Em seguida, adicione, ao menos, uma dimensão de matriz. Você pode especificar o número de linhas ou de colunas com o código nrow = ou ncol =, respectivamente. 

Digamos, por exemplo, que você quer criar uma matriz 2x3 (duas linhas e três colunas) com os valores 3-8. Primeiro, insira um vetor com essa sequência de números: c(3:8) e, em seguida, adicione uma vírgula. Por fim, digite nrow = 2 para especificar o número de linhas. 

matrix(c(3:8), nrow = 2)

Ao executar a função, R exibe uma matriz com três colunas e duas linhas (também conhecida como “2x”3”), que contém os valores numéricos 3, 4, 5, 6, 7, 8. R coloca o primeiro valor (3) do vetor na linha superior e na coluna mais à esquerda da matriz, dando continuidade à sequência da esquerda à direita. 

Você pode especificar o número de colunas (ncol = ) ao invés de linhas (nrow = ). 

matrix(c(3:8), ncol = 2)

Ao executar a função, R automaticamente infere o número de linhas.

## 🧩 Operações

* - [Booleanos](https://libguides.mit.edu/c.php?g=175963&p=1158594)
* **operadores lógicos e instruções condicionais** - [Tutorial](https://www.datacamp.com/community/tutorials/conditionals-and-control-flow-in-r)

## 📦 Pacotes em R disponíveis

* - [BioCondutor](http://bioconductor.org/)
* - [R-Force](https://r-forge.r-project.org/)
* - [rOpenSci](https://ropensci.org/)
* - [CRAN](https://cran.r-project.org/)
* **Leia R Package Primer, de Karl Broman** - [R Package Primer](https://kbroman.org/pkg_primer/)
* **Tidyverse é uma coleção de pacotes de R desenvolvida especialmente para se trabalhar com dados.** - [Tidyverse](https://www.tidyverse.org/)
* **A lista do Suporte do RStudio sobre pacotes úteis com instruções de instalação e descrições de funcionalidades.** - [Listas](https://support.rstudio.com/hc/en-us/articles/201057987-Quick-list-of-useful-R-packages)
* ** Índice de pacotes do CRAN classificados por tarefa. ** - [CRAN Views](https://cran.r-project.org/web/views/)

## 🕹️ Tutoriais

* - [R-Bloggers](https://www.r-bloggers.com/)
* - [Learning](https://www.r-bloggers.com/2015/12/how-to-learn-r-2/#h.y5b98o9o2h1r)


* **** - []()
* **** - []()
* **** - []()



