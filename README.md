# AI benchmark analysis
Analysing AI benchmark performance: generality, ability, difficulty and discrimination

Whenever we confront AI systems with AI problems we are usually interested in the **performance** of the systems and its relation to the **hardness** of the problems. This is a monolithic, usually simplistic, way of looking at results. More dimensions other than performance, some of them latent, could give us a better characterisation of how a set of AI systems behave for a set of AI problems. Here we analyse two indicators on the side of the AI problems, difficulty and discrimination, and two indicators on the side of the AI systems, ability and generality.


## Analysis

We analyse the behaviour of several learning techniques for two of the most popular general-purpose AI benchmarks in the recent years: the **Arcade Learning Environment**, based on the Atari 2600 games and the **General Video Game AI Competition**. We use Item Response Theory, and logistic models in particular, to create item characteristic curves to determine which games in the benchmark are more **difficult** but also more **discriminating**, as well as the **ability** or proficiency of each learning technique addresing the games. These are complemented with a new metric of **generality**.

## Data

**The Arcade Learning Environment** [ALE](https://github.com/mgbellemare/Arcade-Learning-Environment)

One benchmark that has become particularly popular in the past years is the Arcade Learning Environment, a collection of Atari 2600 games that is usually tacked by reinforcement learning algorithms, search methods, planning or a combination, depending on the problem presentation. The popularity of this benchmark has increased significantly since *Mnih et al, 2015* introduced a combination of deep learning and Q-learning, known as DQN, which was able to perform better than humans for many of the games, where learning was just performed by observing the screen and receiving rewards (the score), without any other given representation or description of the game, just learning to play from scratch. 

![ALE data](data/ALE_data.csv)  - ![ALE Scores](plots/Atari.Scores.pdf) 

**The General Video Game AI Competition**  [GVGAI](http://www.gvgai.net/)

Another interesting initiative is the general video game AI (GVGAI) competition, a benchmark which comprises a large number of real-time 2D grid games such as puzzles, shooters and classic arcades. This environment is usually addressed by non-deterministic learning algorithms such as Monte-Carlo Tree Search (MCTS) and Evolutionary Algorithms (EA) . Still as of yet, there has not been an approach able to consistently be successful on all games, showing that all the techniques used have their strengths and weaknesses. 

![GVGAI data](data/GVGAI_data.csv) - ![GVGAI Scores](plots/GVGP.Scores.pdf) 


## Code

- **./data/** Data (csv & rds files) used (Atari and GVGP scores)
- **./plots/** Publication plots
- **AIbenchmark_analysis.R** Complete functionality (ETL processes, IRT 2PL param estimation, generality estimation, publication plots, ...)
- **RunExp_ItemTech.R** Run all the experiments


## Indicators

### [TECHNIQUES] Ability 

![ALE AI techniques](plots/Atari.Ability.pdf)  

![GVGAI AI techniques](plots/GVGP.Ability.pdf) 

### [TECHNIQUES]  Regularity

![ALE AI techniques](plots/Atari.Generality.pdf)  

![GVGAI AI techniques](plots/GVGP.Generality.pdf) 


### [ITEMS] IRT parameters: ALE (Atari 2600 Games)

![ICCs 10 most difficult games](plots/Atari.Diff.pdf) 

![ICCs 10 most discriminant games](plots/Atari.Disc.pdf) 

![ICCs negative discriminant games](plots/Atari.neg.pdf) 


**Parameters**


|Game            | Gussng|      Dffclt|     Dscrmn| id|
|:---------------|------:|-----------:|----------:|--:|
|Amidar          |      0|   1.3953698| 46.1257658|  1|
|Assault         |      0|  -1.3731820|  1.7356477|  2|
|Asterix         |      0|   0.3466661| 58.2687017|  3|
|Atlantis        |      0|  -1.6678197|  3.0735959|  4|
|Bank Heist      |      0|   0.6255703| 39.2330155|  5|
|Battle Zone     |      0|   1.8826677|  3.5741326|  6|
|Beam Rider      |      0|   0.6925208|  1.4294334|  7|
|Boxing          |      0|  -2.6493631|  1.4546294|  8|
|Breakout        |      0|  -1.0159100|  4.9548097|  9|
|Centipede       |      0|  -6.4955923| -0.4083880| 10|
|Chopper Command |      0|   1.8900704|  3.4840278| 11|
|Crazy Climber   |      0|  -1.5324652|  2.9966145| 12|
|Demon Attack    |      0|  -1.2851247|  2.6394376| 13|
|Double Dunk     |      0|  -0.9345613|  2.0808598| 14|
|Enduro          |      0|   0.2227363|  2.1064622| 15|
|Fishing Derby   |      0|   0.6366921|  3.3886737| 16|
|Freeway         |      0|  -0.0242271|  1.2708146| 17|
|Frostbite       |      0|   1.7080332| 53.4460113| 18|
|Gopher          |      0|  -1.3422966| 38.5870993| 19|
|H.E.R.O         |      0|   7.4449250|  0.5151529| 20|
|Ice Hockey      |      0|   1.6096519|  2.9558730| 21|
|James Bond      |      0|  -0.9036758|  3.0071283| 22|
|Kangaroo        |      0|   0.2271428|  1.1614287| 23|
|Kung-Fu Master  |      0|  -0.8189309|  1.9577526| 24|
|Name This Game  |      0|  -0.6603451|  3.6412868| 25|
|Pong            |      0|  -0.6930552| 41.1728252| 26|
|Q*Bert          |      0|   0.7090613|  1.3262309| 27|
|River Raid      |      0|   0.9988255|  3.8493813| 28|
|Road Runner     |      0|  -1.2621008|  3.6072422| 29|
|Robotank        |      0| -10.5160489|  0.1881194| 30|
|Seaquest        |      0|   2.1095352| 19.2966886| 31|
|Space Invaders  |      0|  -0.4221592|  3.7165516| 32|
|Star Gunner     |      0|  -0.7091452| 49.7793349| 33|
|Tennis          |      0|   8.2219425| -0.1336630| 34|
|Time Pilot      |      0|  -0.0154012|  1.5112739| 35|
|Tutankham       |      0|   1.3703207|  0.5813543| 36|
|Up and Down     |      0|  -0.9927637|  1.5674051| 37|
|Venture         |      0|  -5.1580692| -0.6350841| 38|
|Video Pinball   |      0|   2.5415756| -0.2912226| 39|
|Wizard of Wor   |      0|  -0.0983684|  2.3205182| 40|
|Zaxxon          |      0|   0.2398935|  2.8441441| 41|

## [ITEMS] IRT parameters: GVGAI Games

![ICCs 10 most difficult games](plots/GVGP.Diff.pdf) 

![ICCs 10 most discriminant games](plots/GVGP.Disc.pdf) 

![ICCs negative discriminant games](plots/GVGP.neg.pdf) 

**Parameters**

|                  |Gussng|     Dffclt|     Dscrmn| id|
|:-----------------|-----:|----------:|----------:|--:|
|aliens.0          |     0| -1.9846017|  0.9472819|  1|
|aliens.1          |     0| -1.9237777| 11.3019978|  2|
|aliens.2          |     0| -1.5026122|  1.2091678|  3|
|aliens.3          |     0| -1.6946865|  1.7338446|  4|
|aliens.4          |     0| -2.0265812| 62.9567114|  5|
|bait.0            |     0| -8.5741444| -0.0976999|  6|
|bait.1            |     0|  2.3182946| 40.0494905|  7|
|bait.2            |     0|  2.3182946| 40.0494905|  8|
|boulderchase.0    |     0|  2.8448234|  1.8992681|  9|
|boulderchase.1    |     0|  2.8448234|  1.8992681| 10|
|boulderchase.2    |     0|  3.2190826|  0.9269680| 11|
|boulderchase.3    |     0|  2.8448234|  1.8992681| 12|
|boulderchase.4    |     0|  2.1598728|  1.4194350| 13|
|boulderdash.3     |     0|  2.3182946| 40.0494905| 14|
|boulderdash.4     |     0|  1.5889224| 96.8667934| 15|
|brainman.0        |     0|  3.0050612|  0.7626720| 16|
|brainman.4        |     0|  2.3182946| 40.0494905| 17|
|butterflies.0     |     0| -0.6554723| 37.2882522| 18|
|butterflies.1     |     0| -1.1162223|  2.0377885| 19|
|butterflies.2     |     0| -0.6902820| 36.6974569| 20|
|butterflies.3     |     0| -1.4339241|  1.5072263| 21|
|butterflies.4     |     0| -0.8425919|  1.8851186| 22|
|camelRace.0       |     0|-13.8964146| -0.0930378| 23|
|camelRace.1       |     0|-14.1198596| -0.0743290| 24|
|camelRace.2       |     0| 10.5796698|  0.1479168| 25|
|camelRace.3       |     0| -0.4255050|  1.0051979| 26|
|camelRace.4       |     0|  1.7762684|  1.0085417| 27|
|catapults.0       |     0| 84.5264884|  0.0365570| 28|
|catapults.3       |     0| 11.8201041|  0.2010572| 29|
|chase.0           |     0|  1.4772414|  1.6957675| 30|
|chase.1           |     0|  1.3762229| 84.2332892| 31|
|chase.3           |     0|  2.5526899|  1.4501393| 32|
|chipschallenge.0  |     0|  1.2191048|  0.5837330| 33|
|chipschallenge.3  |     0|  2.3182946| 40.0494905| 34|
|crossfire.0       |     0|  1.6465266|  0.8026769| 35|
|crossfire.1       |     0|  1.2911091|  1.4496215| 36|
|crossfire.2       |     0|  2.5526899|  1.4501393| 37|
|crossfire.3       |     0|  1.3762229| 84.2332892| 38|
|crossfire.4       |     0|  2.1598520|  1.4194998| 39|
|defem.0           |     0|  4.3357059|  0.8513857| 40|
|eggomania.0       |     0|  2.3182946| 40.0494905| 41|
|eggomania.1       |     0|  2.3182946| 40.0494905| 42|
|escape.0          |     0|  0.6020281|  1.3487487| 43|
|escape.1          |     0|  1.7690446|  1.0170187| 44|
|escape.2          |     0|  2.2795419|  0.8861553| 45|
|escape.3          |     0|  2.1245502|  2.5957892| 46|
|escape.4          |     0|  1.3708689| 36.5290877| 47|
|factorymanager.3  |     0| 20.8995907| -0.1485438| 48|
|firestorms.0      |     0|  0.3693273|  1.3981555| 49|
|firestorms.1      |     0|  1.1684163|  3.0544923| 50|
|firestorms.2      |     0|  0.1077472|  0.9304494| 51|
|firestorms.3      |     0|  1.6432513|  0.8053592| 52|
|firestorms.4      |     0|  1.0021307|  4.1002985| 53|
|frogs.0           |     0|  0.6140420|  0.4307845| 54|
|frogs.1           |     0|  1.1314351|  0.4049648| 55|
|frogs.2           |     0|  1.7779648|  0.7078291| 56|
|frogs.3           |     0|  4.3350852|  0.8516060| 57|
|frogs.4           |     0|  0.7851131|  0.6502121| 58|
|iceandfire.1      |     0| 84.5264884|  0.0365570| 59|
|infection.0       |     0| -0.4289497|  0.9898807| 60|
|infection.2       |     0| -1.0129394|  0.9477716| 61|
|jaws.0            |     0|  1.2653204|  0.8601310| 62|
|jaws.1            |     0|  2.2465952|  0.5118577| 63|
|jaws.2            |     0|  1.5218202|  0.6330160| 64|
|jaws.3            |     0|  2.8746821|  0.4869372| 65|
|jaws.4            |     0|  1.5218202|  0.6330160| 66|
|labyrinth.0       |     0|  1.0284823|  1.6981585| 67|
|labyrinth.1       |     0|  1.3479830| 34.3971024| 68|
|labyrinth.2       |     0|  1.3479830| 34.3971024| 69|
|labyrinth.3       |     0|  0.6466506|  2.8547372| 70|
|labyrinth.4       |     0|  1.3545695| 78.0335425| 71|
|missilecommand.0  |     0|  1.3677517|  0.7472977| 72|
|missilecommand.1  |     0|  3.0050500|  0.7626762| 73|
|missilecommand.2  |     0|  0.6001069|  1.4347775| 74|
|missilecommand.3  |     0|  3.1709449|  0.9501540| 75|
|modality.0        |     0| -1.7097482|  1.6063514| 76|
|modality.1        |     0|  1.6150693|  1.2628531| 77|
|modality.2        |     0|  2.3182946| 40.0494905| 78|
|modality.4        |     0|  2.3182946| 40.0494905| 79|
|overload.0        |     0|  1.3708689| 36.5290877| 80|
|overload.1        |     0|  1.4438453| 16.4839023| 81|
|overload.2        |     0|  1.3538249| 68.2416120| 82|
|overload.3        |     0|  1.2174654|  1.9538059| 83|
|overload.4        |     0|  0.3732810|  1.4951759| 84|
|pacman.0          |     0|  3.0451150|  0.7478244| 85|
|pacman.1          |     0|  1.6048930|  2.3123649| 86|
|pacman.2          |     0|  2.5292783|  1.0121123| 87|
|pacman.3          |     0|  1.3687374|  2.5655930| 88|
|pacman.4          |     0|  1.3762229| 84.2332892| 89|
|painter.0         |     0| -1.1483332|  1.5209729| 90|
|painter.1         |     0|  1.3540402| 70.8217667| 91|
|painter.2         |     0|  0.3819716|  1.6629455| 92|
|painter.3         |     0|  0.1323497|  1.3843284| 93|
|painter.4         |     0| -2.1632328|  1.1882606| 94|
|plants.1          |     0|  4.3350852|  0.8516060| 95|
|plaqueattack.0    |     0|  1.1793420|  1.0020522| 96|
|plaqueattack.1    |     0|  0.3669707|  1.1174288| 97|
|plaqueattack.2    |     0|  0.3663167|  1.2771911| 98|
|plaqueattack.3    |     0|  0.8141506|  2.0911354| 99|
|plaqueattack.4    |     0|  1.0035449|  2.3536737|100|
|portals.0         |     0|  1.2911091|  1.4496215|101|
|portals.1         |     0|  0.3820316|  1.6636843|102|
|portals.2         |     0|  0.8141709|  2.0988474|103|
|portals.3         |     0|  0.9860240|  0.8221371|104|
|portals.4         |     0|  2.5526899|  1.4501393|105|
|racebet2.0        |     0|  1.1770715|  1.0066045|106|
|racebet2.1        |     0| -0.3424664|  0.3297765|107|
|racebet2.2        |     0|  2.0430807|  0.4322245|108|
|racebet2.3        |     0|  1.3125207|  0.3427405|109|
|racebet2.4        |     0|-29.5821281| -0.0528601|110|
|roguelike.1       |     0|  2.8448234|  1.8992681|111|
|roguelike.2       |     0|  2.8448234|  1.8992681|112|
|roguelike.3       |     0|  2.8448234|  1.8992681|113|
|roguelike.4       |     0|  2.8448234|  1.8992681|114|
|seaquest.0        |     0|  2.3948228|  0.2657695|115|
|seaquest.1        |     0|  2.0426985|  0.4322659|116|
|seaquest.2        |     0| 20.2471695|  0.0631874|117|
|seaquest.3        |     0|  4.8614933|  0.2695578|118|
|seaquest.4        |     0|  4.8698744|  0.2691135|119|
|sokoban.0         |     0|  2.3182946| 40.0494905|120|
|sokoban.1         |     0|  2.3182946| 40.0494905|121|
|sokoban.2         |     0|  2.3182946| 40.0494905|122|
|sokoban.4         |     0|  1.1452920|  0.6386277|123|
|solarfox.0        |     0| -1.2959714|  0.9856755|124|
|solarfox.1        |     0| -0.2304539|  0.5811183|125|
|solarfox.2        |     0| -0.2304878|  0.5810540|126|
|solarfox.3        |     0|  0.1167547|  0.6518909|127|
|solarfox.4        |     0|  1.1141112|  1.1709375|128|
|survivezombies.0  |     0| -1.6785155|  0.4282813|129|
|survivezombies.1  |     0|  0.4075599|  0.7722223|130|
|survivezombies.2  |     0| -2.4176105| -0.1918030|131|
|tercio.0          |     0|  2.3182946| 40.0494905|132|
|tercio.3          |     0|  2.3182946| 40.0494905|133|
|thecitadel.0      |     0|  1.0531034|  1.4640539|134|
|thecitadel.2      |     0|  2.3182946| 40.0494905|135|
|thecitadel.3      |     0|  1.3872013| 50.5351879|136|
|thecitadel.4      |     0|  2.3182946| 40.0494905|137|
|waitforbreakfast.0|     0|  0.8292580|  1.4141355|138|
|waitforbreakfast.1|     0|  0.6467670|  2.8578066|139|
|waitforbreakfast.2|     0|  1.2198811|  1.9271879|140|
|waitforbreakfast.3|     0|  0.6795320|123.2199669|141|
|waitforbreakfast.4|     0|  0.6845985|  0.8302034|142|
|whackamole.0      |     0|  0.3896921|  1.7920152|143|
|whackamole.1      |     0|  0.1161534|  0.6580553|144|
|whackamole.2      |     0| -0.1148338|  1.3455490|145|
|whackamole.4      |     0|  1.2618959|  1.5985370|146|
|zelda.0           |     0|  1.8661834|  2.2672749|147|
|zelda.1           |     0|  1.4438453| 16.4839023|148|
|zelda.2           |     0|  2.5531674|  1.4492640|149|
|zelda.3           |     0|  1.3675078| 52.3842493|150|
|zelda.4           |     0|  1.4771980|  1.6958043|151|
|zenpuzzle.0       |     0| 11.9193644|  0.1993375|152|
|zenpuzzle.1       |     0|  8.8564733|  0.3613949|153|
|zenpuzzle.3       |     0|  1.2044786|  0.9536603|154|




