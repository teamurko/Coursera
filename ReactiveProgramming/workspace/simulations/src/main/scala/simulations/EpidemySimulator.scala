package simulations

import math.random

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt

  protected[simulations] object SimConfig {
    val HEALTHY_POPULATION: Int = 99
    val INFECTED_POPULATION: Int = 1
    val population = HEALTHY_POPULATION + INFECTED_POPULATION
    val ROOM_ROWS: Int = 8
    val ROOM_COLUMNS: Int = 8
    val roomColumns = ROOM_COLUMNS
    val roomRows = ROOM_ROWS
    val MAX_MOVE_DELAY: Int = 4
    val upTime: Int = 10
    val TRANS_RATE: Double = 0.1  
    val DEATH_RATE: Double = 0.25
    val INCUBATION_PERIOD = 6
    val PERIOD_BEFORE_DEATH = 8
    val PERIOD_BEFORE_IMMUNE = 2
    val PERIOD_BEFORE_HEALTH = 2

    // to complete: additional parameters of simulation
  }

  import SimConfig._
  
  def infectedPeople: List[Person] = {
    def make(count : Int): List[Person] = {
      if (count == 0) Nil
      else {
        val person = new Person(-count)
        person.makeInfected
        person.infected = true
        person::make(count - 1)
      }
    }
    make(INFECTED_POPULATION)
  }
  
  def healthyPeople: List[Person] = {
    def make(count : Int): List[Person] = {
      if (count == 0) Nil
      else {
        val person = new Person(count)
        person::make(count - 1)
      }
    }
    make(HEALTHY_POPULATION)
  }

  def makePopulation = {
    val people = healthyPeople:::infectedPeople
    people.foreach(p => p.addMoveAction)
    people
  }
  
  val persons: List[Person] = makePopulation
		  									
  class Person (val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false

    // demonstrates random number generation
    var row: Int = randomBelow(ROOM_ROWS)
    var col: Int = randomBelow(ROOM_COLUMNS)

    def hasVisiblyInfectious(row: Int, col: Int) = {
      persons.exists(p => (p.row == row && p.col == col) && (p.dead || p.sick))
    }
    
    def hasInfectious(row: Int, col: Int) = {
      persons.exists(p => (p.row == row && p.col == col) && p.infected)
    }
    
    def getInfected = {
      random < TRANS_RATE
    }
    
    def getDead = {
      random < DEATH_RATE
    }
    
    def getMoveDelay = {
      randomBelow(MAX_MOVE_DELAY) + 1
    }
    
    def adjacentRooms(row: Int, col: Int) = {
      val shifts = (-1,0)::(1,0)::(0,-1)::(0,1)::Nil
      val rooms = shifts.map(xy =>
        		((row + xy._1 + ROOM_ROWS) % ROOM_ROWS, (col + xy._2 + ROOM_COLUMNS) % ROOM_COLUMNS))
      rooms.filter(rc => !hasVisiblyInfectious(rc._1, rc._2))
    }
    
    def makeInfected = {
      addGetInfectedAction
    }
    
    def addGetInfectedAction: Unit = {
      def getInfectedAction {
        infected = true
        addGetSickAction
      }
      afterDelay(0) { getInfectedAction }
    }
    
    def addGetSickAction: Unit = {
      def getSickAction {
        sick = true
        addGetDeadAction
      }
      afterDelay(INCUBATION_PERIOD) { getSickAction }
    }
    
    def addGetImmuneAction: Unit = {
      def getImmuneAction {
        sick = false
        immune = true
        addGetHealthyAction
      }
      afterDelay(PERIOD_BEFORE_IMMUNE) { getImmuneAction }
    }
    
    def addGetHealthyAction: Unit = {
      def getHealthyAction {
        immune = false
        infected = false
      }
      afterDelay(PERIOD_BEFORE_HEALTH) { getHealthyAction }
    }
    
    def addGetDeadAction: Unit = {
      def getDeadAction {
        if (getDead) {
          sick = false
          dead = true
        } else {
          addGetImmuneAction
        }
      }
      afterDelay(PERIOD_BEFORE_DEATH) { getDeadAction }
    }
    
    def addMoveAction: Unit = {
      def moveAction() {
        if (dead) return
        val rooms = adjacentRooms(row, col).toArray
        if (rooms.length > 0) {
          val i = randomBelow(rooms.length)
          row = rooms(i)._1
          col = rooms(i)._2
        }
        if (!infected) {
          if (hasInfectious(row, col) && getInfected) {
            addGetInfectedAction
          }
        }
        afterDelay(getMoveDelay) { moveAction }
      }
      afterDelay(getMoveDelay) { moveAction }
    }
  }
}
