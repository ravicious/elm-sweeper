'use strict';

var node = document.getElementById('app-container')
var randomNumber = Math.floor(Math.random()*0xFFFFFFFF)
var app = Elm.Main.init({
  node: node,
  flags: {
    randomNumber: randomNumber,
  },
})

window.initializeWithSeed = function(seed) {
  app.ports.initializeWithSeed.send(seed)
}

window.initializeWithRandomSeed = function() {
  var randomNumber = Math.floor(Math.random()*0xFFFFFFFF)

  app.ports.initializeWithSeed.send(randomNumber)
}

app.ports.gameHasBeenLost.subscribe(function() {
  // We can't display the popup immediately, because the player won't be able to see the effect of
  // their last action.
  // That's because `window.confirm` will block rendering.
  //
  // The timeout has to be greater than 1, because when set to one, it doesn't make any difference
  // in Safari.
  window.setTimeout(function() {
    var wantsToPlayAgain = window.confirm('Game over! Want to play again?')

    wantsToPlayAgain && initializeWithRandomSeed()
  }, 100)
})

var hitByMonsterTimeoutId, levelUpTimeoutId;

app.ports.emitGameEvents.subscribe(function(gameEvents) {
  gameEvents.forEach(function(gameEvent) {
    switch (gameEvent) {
      case 'HitByMonster':
        // If already happening, just make the animation last longer.
        if (hitByMonsterTimeoutId) {
          clearTimeout(hitByMonsterTimeoutId)
        } else {
          document.body.classList.add('shaking')
        }

        hitByMonsterTimeoutId = window.setTimeout(function() {
          document.body.classList.remove('shaking')
          hitByMonsterTimeoutId = undefined
        }, 600)
        break
      case 'LevelUp':
        window.requestAnimationFrame(function() {
          // If already happening, interrupt the animation.
          if (levelUpTimeoutId) {
            clearTimeout(levelUpTimeoutId)
            // Without those two calls requestAnimationFrame, removing a class and then just
            // adding it again wouldn't really trigger the animation. So first we issue one
            // requestAnimationFrame to do the class removal if needed. Inside another animation
            // frame we add the class again, guaranteeing that the animation will be interrupted.
            document.getElementById('grid').classList.remove('green-flash')
          }

          window.requestAnimationFrame(function() {
            document.getElementById('grid').classList.add('green-flash')

            levelUpTimeoutId = window.setTimeout(function() {
              document.getElementById('grid').classList.remove('green-flash')
              levelUpTimeoutId = undefined
            }, 1500)
          })
        })
        break
    }
  })
})

document.onkeydown = function(event) {
  var cell = document.querySelectorAll('.grid-cell:hover')[0]
  var index = cell && parseInt(cell.dataset.index, 10) || null

  app.ports.keyDown.send([event.code, index])
}

document.onkeyup = function(event) {
  var cell = document.querySelectorAll('.grid-cell:hover')[0]
  var index = cell && parseInt(cell.dataset.index, 10) || null

  app.ports.keyUp.send([event.code, index])
}
