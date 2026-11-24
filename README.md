# DistrNormBivar — La distribution normale bivariée

**DistrNormBivar** est une application Shiny pédagogique, en français, pour explorer la loi normale bivariée : sa surface de densité, ses courbes de niveau et ses distributions conditionnelles.

- **Application en ligne :** <https://shiny.dcstats.org/DistrNormBivar/>

## Le principe

On règle les deux moyennes, les deux écarts-types et la corrélation, et la distribution se redessine aussitôt, en surface 3D interactive ou en courbes de niveau. L'application fait voir ce que chaque paramètre change :

- la corrélation règle l'orientation et le resserrement de l'ellipse;
- les écarts-types règlent son échelle sur chaque axe;
- la hauteur de la surface est une densité, pas une probabilité : une probabilité correspond à un volume sous la surface.

## Contenu

- **Introduction théorique** : la loi conjointe, la fonction de densité et ses cinq paramètres, avec une mise en garde : des marginales normales ne garantissent pas une loi normale bivariée.
- **Distribution bivariée** :
  - surface 3D et courbes de niveau (plotly);
  - réglage par sliders ou par saisie numérique;
  - choix de palettes de couleurs et affichage des isolignes;
  - un échantillon simulé (50, 200 ou 500 observations) superposé à la surface.
- **Distribution conditionnelle** :
  - on fixe X = x et l'on voit la distribution de Y | X = x, une « coupe » de la surface;
  - la droite de régression, la moyenne et l'écart-type conditionnels;
  - le calcul de P(Y > seuil | X = x), avec une interprétation commentée.
- **Laboratoire guidé** : quatre expériences, chacune avec prédiction, réglages appliqués en un clic, puis vérification :
  - indépendance;
  - association positive forte;
  - association négative forte;
  - même corrélation, autre échelle.

## Exécuter l'application localement

```r
install.packages(c("shiny", "bslib", "plotly", "mvtnorm", "colourpicker"))
shiny::runApp("DistrNormBivar")
```

## Auteur

Daniel Coulombe, Ph.D. — <https://dcstats.org>
