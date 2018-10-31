# Ship analysis tool for Eclipse boardgame

Like playing [Eclipse boardgame](https://boardgamegeek.com/boardgame/72125/eclipse), but suck at estimating how to improve your fleet? The goal of this side project was to help you (me) to do just that?

## Current features:
- Actually calculates probabilities, maybe even correct ones :)
- Calculates results for Ion, Plasma and Antimatter Cannons as well as Plasma Missiles.
- Takes into account Computers and Shields (in case target ship has some)

## Currently not features:
- Does not validate ship design
- Rare parts are not available for selection
- Does take into account initiative

Maybe I'll in the implement them in the future, but probably I'm just lazy and will move to some other project. Or maybe present results for all ship types (at least expected damages). 

## Technical detail

The main point was to write this small web app with Haskell. In this case I used [Scotty](https://github.com/scotty-web/scotty) as the web framework. I used [blaze-html](https://hackage.haskell.org/package/blaze-html) for templating html. I have a bit mixed feelings about it, as it some times felt a bit cumbersome, but then again the first time I didn't mess up closing html divs. 

The results are rendered with [Chart.js](http://www.chartjs.org/) with the help of Bootstrap and jQuery. I wrote some ugly js to do the plumbing. The actual result calculation is done in the web server, although it would make more sense to do them with Javascript, but then I would have ended with static web page and even uglier Javascript. Where's the fun in that?

The math side uses binomial distribution for calculating. Not very fancy math, but at least it's not simulated. Actually the math is so simple, that I kind of presume I made a mistake somewhere :D.
