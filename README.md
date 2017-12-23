# AI benchmark analysis
Analysing AI benchmark performance: generality, ability, difficulty and discrimination

## Code

- **./Data/** Data (csv & rds files) used (Atari and GVGP scores)
- **./Plots/** Publication plots
- **AIbenchmark_analysis.R** Complete functionality (ETL processes, IRT 2PL param estimation, generality estimation, publication plots, ...)
- **RunExp_ItemTech.R** Run all the experiments

## Analysis

### IRT parameters: Atari 2600 Games

![ALL ICCs](plots/Atari.all.pdf) 
![Image of Yaktocat](https://octodex.github.com/images/yaktocat.png)


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

## IRT parameters: GVGP Games


|Game.Level         | Gussng|      Dffclt|      Dscrmn|  id|
|:------------------|------:|-----------:|-----------:|---:|
|aliens.0           |      0|  -0.4026351|   3.3304192|   1|
|aliens.1           |      0|  -0.3562627|   2.1555121|   2|
|aliens.2           |      0|  -0.4026351|   3.3304192|   3|
|aliens.3           |      0|  -0.3017344|   1.3639965|   4|
|aliens.4           |      0|  -0.3562627|   2.1555121|   5|
|bait.0             |      0|  11.7488889|  -0.0073510|   6|
|bait.1             |      0|   3.8160548|  -0.1138259|   7|
|bait.2             |      0|   0.4062375|  -0.1767384|   8|
|bait.3             |      0|  -2.5808379|   0.0350069|   9|
|bait.4             |      0|  -2.5808379|   0.0350069|  10|
|boloadventures.0   |      0|   0.3248966|  -0.7362017|  11|
|boloadventures.1   |      0|  72.2868962|  -0.0427348|  12|
|boloadventures.2   |      0|  72.2868962|  -0.0427348|  13|
|boloadventures.3   |      0|   0.1221295|  -0.4198033|  14|
|boloadventures.4   |      0|   0.5739930|  -0.1316140|  15|
|boulderchase.0     |      0|  -0.4024370|   3.3283400|  16|
|boulderchase.1     |      0|  -0.2764184|   0.7713990|  17|
|boulderchase.2     |      0|  -0.3016781|   1.3631837|  18|
|boulderchase.3     |      0|  -0.2891576|   0.5933811|  19|
|boulderchase.4     |      0|  -0.3561179|   2.1543585|  20|
|boulderdash.0      |      0|  -0.3049981|   0.5055048|  21|
|boulderdash.1      |      0|  13.1408384|  -0.1446758|  22|
|boulderdash.2      |      0|  -0.3561636|   2.1550847|  23|
|boulderdash.3      |      0|  -0.5219683|   1.0920644|  24|
|boulderdash.4      |      0|  -0.3015097|   1.3608612|  25|
|brainman.0         |      0|  -0.5552224|   0.8194648|  26|
|brainman.1         |      0|  -0.2763143|   0.7700185|  27|
|brainman.2         |      0|  -0.2891291|   0.5937295|  28|
|brainman.3         |      0|  -0.3020407|   0.5182595|  29|
|brainman.4         |      0|   0.5139717|  -0.4721046|  30|
|butterflies.0      |      0|  -0.4024436|   3.3285500|  31|
|butterflies.1      |      0|  -0.3234002|   1.6568055|  32|
|butterflies.2      |      0|  -0.3561781|   2.1549496|  33|
|butterflies.3      |      0|  -0.3560573|   2.1539741|  34|
|butterflies.4      |      0|  -0.2878817|   0.6049363|  35|
|camelRace.0        |      0|   0.2438219|  -0.9518802|  36|
|camelRace.1        |      0|   0.6253010|  -0.7509931|  37|
|camelRace.2        |      0|   0.2438721|  -0.9517664|  38|
|camelRace.3        |      0|   5.1970287|  -0.1197237|  39|
|camelRace.4        |      0|   0.1302599|  -1.4396067|  40|
|catapults.0        |      0|   0.8189240|  -0.3000277|  41|
|catapults.1        |      0|   0.1182586|  -0.4276852|  42|
|catapults.2        |      0|   0.0666732|  -0.5607392|  43|
|catapults.3        |      0|   0.1360231|  -1.4090436|  44|
|catapults.4        |      0|   0.2680545|  -0.2464902|  45|
|chase.0            |      0|  -0.3017037|   1.3631421|  46|
|chase.1            |      0|  -0.2795468|   1.0055363|  47|
|chase.2            |      0|  -0.3017037|   1.3631421|  48|
|chase.3            |      0|  -0.4512205|   0.2543546|  49|
|chase.4            |      0|  -0.2803852|   0.6791253|  50|
|chipschallenge.0   |      0|  -0.5266975|   2.0095654|  51|
|chipschallenge.1   |      0|  -0.2762876|   0.7712752|  52|
|chipschallenge.2   |      0|  -1.0868036|   0.2762548|  53|
|chipschallenge.3   |      0|  -0.5823279|   0.7171710|  54|
|chipschallenge.4   |      0|  -0.8565823|   0.7790210|  55|
|crossfire.0        |      0|   0.8098639|  -0.3032956|  56|
|crossfire.1        |      0|   0.1169786|  -0.4300056|  57|
|crossfire.2        |      0|   1.0025185|  -1.1237414|  58|
|crossfire.3        |      0|   0.5134184| -37.8175235|  59|
|crossfire.4        |      0|   1.1464008|  -1.6215183|  60|
|defem.0            |      0|   0.9174510|  -0.0864965|  61|
|defem.1            |      0|  -0.2873824|   1.1581887|  62|
|defem.2            |      0|  -0.3556624|   2.1450227|  63|
|defem.3            |      0|  -0.2795911|   1.0042660|  64|
|defem.4            |      0|  -0.3059393|   0.5014802|  65|
|digdug.0           |      0|  -0.5158535|   1.2851119|  66|
|digdug.1           |      0|  -0.2795925|   1.0056421|  67|
|digdug.2           |      0|  -0.2764470|   0.7725768|  68|
|digdug.3           |      0|  -0.2763229|   0.7725555|  69|
|digdug.4           |      0|  -0.5348624|   0.9430430|  70|
|eggomania.0        |      0|   0.1209134|  -0.4221972|  71|
|eggomania.1        |      0|   0.2049970|  -0.3006417|  72|
|eggomania.2        |      0|   0.6180261|  -0.1232726|  73|
|eggomania.3        |      0|   0.1093794|  -0.4464401|  74|
|eggomania.4        |      0|   0.0615124|  -0.5780120|  75|
|escape.0           |      0|   2.7191081|  -0.6613570|  76|
|escape.1           |      0|  -0.0620487|  -1.2185945|  77|
|escape.2           |      0|   1.6282636|  -0.7623525|  78|
|escape.3           |      0|   1.1592147|  -0.5793932|  79|
|escape.4           |      0|   2.2144552|  -0.3846465|  80|
|factorymanager.0   |      0|  -1.1209832|   1.5817229|  81|
|factorymanager.1   |      0|  -2.6421313|   0.5801787|  82|
|factorymanager.2   |      0|   3.1129556|  -0.4357332|  83|
|factorymanager.3   |      0|  11.9235882|  -0.0218332|  84|
|factorymanager.4   |      0|  -1.1947720|   1.2589646|  85|
|firecaster.0       |      0|  -0.8822510|   1.9816579|  86|
|firecaster.1       |      0|  -0.6929451|   2.8562575|  87|
|firecaster.2       |      0|  -0.3548905|   0.3691429|  88|
|firecaster.3       |      0|  -0.4025885|   3.3308266|  89|
|firecaster.4       |      0|  -0.5266971|   2.0094891|  90|
|firestorms.0       |      0|   1.1464008|  -1.6215183|  91|
|firestorms.1       |      0|   0.4238233|  -1.2150915|  92|
|firestorms.2       |      0|   1.0358654|  -1.0571235|  93|
|firestorms.3       |      0|   1.0402420|  -0.4229783|  94|
|firestorms.4       |      0|   0.1640070|  -0.3502518|  95|
|frogs.0            |      0|   0.0900260|  -1.6703229|  96|
|frogs.1            |      0|  -0.0000026| -38.6092970|  97|
|frogs.2            |      0|  -0.0000026| -38.6092970|  98|
|frogs.3            |      0|  -0.0659955|  -3.0945459|  99|
|frogs.4            |      0|   0.9352559|  -0.4743311| 100|
|iceandfire.0       |      0|  -0.9218023|   1.5163272| 101|
|iceandfire.1       |      0|  -1.7507171|   1.8759978| 102|
|iceandfire.2       |      0|  -0.3041167|   0.5089176| 103|
|iceandfire.3       |      0|  -0.5159446|   1.2848723| 104|
|iceandfire.4       |      0|  -0.2763975|   0.7719900| 105|
|infection.0        |      0|  -0.8558391|   0.7802024| 106|
|infection.1        |      0|  -0.3561781|   2.1549496| 107|
|infection.2        |      0|  -0.5305086|   0.2047888| 108|
|infection.3        |      0|  -0.3561568|   2.1550568| 109|
|infection.4        |      0|  -0.2762996|   0.8957373| 110|
|jaws.0             |      0|  -0.4009651|   0.3024914| 111|
|jaws.1             |      0|  -0.4540028|   0.2523066| 112|
|jaws.2             |      0|  -0.4009651|   0.3024914| 113|
|jaws.3             |      0|  -0.2764427|   0.7714981| 114|
|jaws.4             |      0|  -0.2764157|   0.7715481| 115|
|labyrinth.0        |      0|   0.1647560|  -0.3491083| 116|
|labyrinth.1        |      0|   4.9789795|  -0.7105549| 117|
|labyrinth.2        |      0|   6.4476478|  -0.3015978| 118|
|labyrinth.3        |      0|  -0.0620436|  -1.2185860| 119|
|labyrinth.4        |      0|   1.2973562|  -1.1402690| 120|
|lemmings.0         |      0|  -0.3383516| -86.7535086| 121|
|lemmings.1         |      0|  -0.3383516| -86.7535086| 122|
|lemmings.2         |      0|  -0.3383516| -86.7535086| 123|
|lemmings.3         |      0|   0.2653955|  -1.9891760| 124|
|lemmings.4         |      0|  -0.0000026| -38.6092970| 125|
|missilecommand.0   |      0|   0.0000208|  -0.8442652| 126|
|missilecommand.1   |      0|  -0.5157728|   1.2861784| 127|
|missilecommand.2   |      0|   0.2211108|  -0.2845689| 128|
|missilecommand.3   |      0|   2.6715024|  -0.1620669| 129|
|missilecommand.4   |      0|  -0.7070917|  55.4219591| 130|
|modality.0         |      0|  -0.2877926|   1.1604695| 131|
|modality.1         |      0|  -1.4693960|   2.0462993| 132|
|modality.2         |      0|  -1.3069612|   1.7324534| 133|
|modality.3         |      0|  -1.4693960|   2.0462993| 134|
|modality.4         |      0|  -1.5988485|   2.8226599| 135|
|overload.0         |      0|  -1.1950671|   1.2580325| 136|
|overload.1         |      0|  -1.7162925|   0.4257599| 137|
|overload.2         |      0|  -0.2757603|   0.8523591| 138|
|overload.3         |      0|  -0.3561011|   2.1539673| 139|
|overload.4         |      0|  -0.3562955|   2.1552669| 140|
|pacman.0           |      0|  -0.3559766|   2.1499743| 141|
|pacman.1           |      0|  -0.6507951|  59.0482818| 142|
|pacman.2           |      0|  -0.5437705|   2.9729328| 143|
|pacman.3           |      0|  -0.4026243|   3.3307926| 144|
|pacman.4           |      0|  -0.5437705|   2.9729328| 145|
|painter.0          |      0|  -0.3018947|   1.3667422| 146|
|painter.1          |      0|  -0.6502436|  57.8806987| 147|
|painter.2          |      0|  -0.5264689|   2.0009351| 148|
|painter.3          |      0|  -0.4024299|   3.3280268| 149|
|painter.4          |      0|   0.3824184|  -0.1857862| 150|
|plants.0           |      0|   0.9557387|  -0.0832977| 151|
|plants.1           |      0|   0.9557387|  -0.0832977| 152|
|plants.2           |      0|   0.9557387|  -0.0832977| 153|
|plants.3           |      0|   0.5501973|  -0.1365539| 154|
|plants.4           |      0|   0.2021226|  -0.3037126| 155|
|plaqueattack.0     |      0|  -1.0972315|   0.0870183| 156|
|plaqueattack.1     |      0|  -0.4550305|   0.2515443| 157|
|plaqueattack.2     |      0|  -1.0943398|   0.0872338| 158|
|plaqueattack.3     |      0|  -1.0943398|   0.0872338| 159|
|plaqueattack.4     |      0|  -1.0972315|   0.0870183| 160|
|portals.0          |      0|  -3.6543478|   0.8033282| 161|
|portals.1          |      0|  -4.7116729|   0.0188375| 162|
|portals.2          |      0|  -2.8573686|   0.8792237| 163|
|portals.3          |      0|  -2.2707476|   0.9748162| 164|
|portals.4          |      0|  -2.8546223|   0.8806601| 165|
|racebet2.0         |      0|  -2.5947884|   0.0347569| 166|
|racebet2.1         |      0|  -1.8549127|   0.1509124| 167|
|racebet2.2         |      0|  -2.6184230|   0.0344585| 168|
|racebet2.3         |      0|   0.2639455|  -0.2494317| 169|
|racebet2.4         |      0|   1.1547062|  -0.0699353| 170|
|realportals.0      |      0|  -1.6550879|   0.2971721| 171|
|realportals.3      |      0|  -1.2563926|   0.4184055| 172|
|realportals.4      |      0|  -0.2763898|   0.7720855| 173|
|realsokoban.0      |      0|  27.7119112|  -0.0094355| 174|
|realsokoban.2      |      0|  -0.4523808|   0.2534424| 175|
|realsokoban.3      |      0|   0.3359973|  -0.7131040| 176|
|roguelike.0        |      0|  -0.6850007|   0.5205718| 177|
|roguelike.1        |      0|  -0.4024829|   3.3285492| 178|
|roguelike.2        |      0|  -0.4026638|   3.3306241| 179|
|roguelike.3        |      0|  -0.3557053|   2.1458980| 180|
|roguelike.4        |      0|  -1.0975144|   0.0869446| 181|
|seaquest.0         |      0|  -0.3561568|   2.1550568| 182|
|seaquest.1         |      0|  -0.4026638|   3.3306241| 183|
|seaquest.2         |      0|  -0.3560866|   2.1540845| 184|
|seaquest.3         |      0|  -0.3561128|   2.1541324| 185|
|seaquest.4         |      0|  -0.3561691|   2.1550256| 186|
|sokoban.1          |      0|   0.1521283|  -0.3677023| 187|
|sokoban.2          |      0|   1.0345038|  -0.0774487| 188|
|sokoban.3          |      0|  -2.8452719|   0.2353424| 189|
|sokoban.4          |      0|  -0.6194396|   0.6257047| 190|
|solarfox.0         |      0|  -0.2890143|   0.5945712| 191|
|solarfox.1         |      0|  -0.3285736|   0.4273138| 192|
|solarfox.2         |      0|  -0.7989593|   0.1244014| 193|
|solarfox.3         |      0|  -0.3283590|   0.4277649| 194|
|solarfox.4         |      0|  -0.3583288|   0.3628363| 195|
|superman.0         |      0|  -0.2798093|   1.0077987| 196|
|superman.1         |      0|  -0.2798093|   1.0077987| 197|
|superman.2         |      0|  -0.3243122|   0.4385811| 198|
|superman.3         |      0|  -0.2887694|   0.5965904| 199|
|superman.4         |      0|  -0.3019247|   0.5196474| 200|
|surround.0         |      0|  -2.1993042|   0.7624293| 201|
|surround.1         |      0|   0.1981797|  -0.3079110| 202|
|surround.2         |      0|  -4.8852430|   0.3423957| 203|
|surround.3         |      0|   1.3181542|  -0.1893896| 204|
|surround.4         |      0|  -3.8480406|   0.3627075| 205|
|survivezombies.0   |      0|  -0.3475456|   0.3833427| 206|
|survivezombies.1   |      0|   4.7196799|  -0.0180840| 207|
|survivezombies.2   |      0|  -0.3020936|   0.5185594| 208|
|survivezombies.3   |      0|  -0.7932662|   0.1254399| 209|
|survivezombies.4   |      0|  -0.3798125|   0.3294785| 210|
|tercio.0           |      0|  -4.9395480|   0.7380471| 211|
|tercio.2           |      0|  -4.9395480|   0.7380471| 212|
|tercio.3           |      0|  -4.9395480|   0.7380471| 213|
|thecitadel.0       |      0|   0.6153418|  -0.1237662| 214|
|thecitadel.1       |      0|  -1.9437581|   0.1434539| 215|
|thecitadel.2       |      0|   2.1441822|  -0.1181553| 216|
|thecitadel.3       |      0|  -1.9434307|   0.1434741| 217|
|thecitadel.4       |      0|  -0.3473846|   0.3836793| 218|
|waitforbreakfast.0 |      0| -10.9521248|   0.2923672| 219|
|waitforbreakfast.1 |      0| -10.9521248|   0.2923672| 220|
|waitforbreakfast.2 |      0| -10.9521248|   0.2923672| 221|
|waitforbreakfast.3 |      0| -10.9521248|   0.2923672| 222|
|whackamole.0       |      0|  -1.2924332|   0.2254162| 223|
|whackamole.1       |      0|  -0.3528810|   0.3728234| 224|
|whackamole.2       |      0|  -0.2890265|   0.5940636| 225|
|whackamole.3       |      0|  -0.3015754|   1.3634328| 226|
|whackamole.4       |      0|   0.0434088|  -0.6429968| 227|
|zelda.0            |      0|  -0.3467583|   0.3843012| 228|
|zelda.1            |      0|  -4.4248562|   0.1947063| 229|
|zelda.2            |      0|  -2.0415713|   0.2337881| 230|
|zelda.3            |      0|  -0.4307014|   0.2717335| 231|
|zelda.4            |      0|  -1.6618897|   0.2957929| 232|
|zenpuzzle.0        |      0|  -0.2796525|   1.0068945| 233|
|zenpuzzle.1        |      0|  -0.2796525|   1.0068945| 234|
|zenpuzzle.2        |      0|  -0.2925847|   0.5703631| 235|
|zenpuzzle.3        |      0|  -0.2891768|   0.5942216| 236|
|zenpuzzle.4        |      0|  -0.2761978|   0.8807376| 237|
