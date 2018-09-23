'use strict'

var node = document.getElementById('app-container')
var randomNumber = Math.floor(Math.random() * 0xFFFFFFFF)
var app = Elm.Main.embed(node, {
  randomNumber: randomNumber,
})

window.initializeWithSeed = function(seed) {
  app.ports.initializeWithSeed.send(seed)
}

window.initializeWithRandomSeed = function() {
  var randomNumber = Math.floor(Math.random() * 0xFFFFFFFF)

  app.ports.initializeWithSeed.send(randomNumber)
}

app.ports.gameHasBeenLost.subscribe(function() {
  // We can't display the popup immediately, because the player
  // won't be able to see the effect of their last action.
  // That's because `window.confirm` will block rendering.
  //
  // The timeout has to be greater than 1, because when set to one,
  // it doesn't make any difference in Safari.
  window.setTimeout(function() {
    var wantsToPlayAgain = window.confirm('Game over! Want to play again?')

    wantsToPlayAgain && initializeWithRandomSeed()
  }, 100)
})

document.onkeydown = function(event) {
  var cell = document.querySelectorAll('.grid-cell:hover')[0]

  if (!cell) {
    return
  }

  const index = parseInt(cell.dataset.index, 10)

  app.ports.keyPressedOverCell.send([index, event.code])
}

setTimeout(function() {
  var grid = document.getElementsByClassName('grid')[0]

  var mc = new Hammer.Manager(grid)
  mc.add(new Hammer.Tap())
  mc.add(new Hammer.Press())

  mc.on('tap', function(event) {
    console.log("Tap!", event)
    event.preventDefault()
  })
  mc.on('press', function(event) {
    console.log("Press!", event)
    event.preventDefault()
  })
}, 500)
