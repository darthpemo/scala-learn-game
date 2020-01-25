package test

import cats.data.NonEmptyList

class Simulation {

}

sealed class GameEvent

case class PlayerJoined(
	name: String
)

case class CreepSpawned(
	name    : String,
	location: (Int, Int)
)

sealed class GameCommand

case class JoinGame(name: String) extends GameCommand

case class PlaceCreep(name: String) extends GameCommand

case class Creep(
	name: String,
	location: (Int, Int) = (0, 0)
)

case class GameState(
	step   : Int = 0,
	players: List[String] = List(),
	creeps : List[Creep] = List()
)

object Simulator {
	def processCommand(gameCommand: GameCommand, gameState: GameState, events: List[GameEvent]): (GameState, List[GameEvent]) = {
		gameCommand match {
			case a: JoinGame =>
				println(s"Player ${a.name} has joined the game")
				(gameState.copy(players = a.name :: gameState.players), events)
			case b: PlaceCreep =>
				println(s"Creep spotted - ${b.name}")
				(gameState.copy(creeps = Creep(b.name, (0, 10)) :: gameState.creeps), events)
		}
	}

	@scala.annotation.tailrec
	def processCommands(gameCommands: NonEmptyList[GameCommand], gameState: GameState, events: List[GameEvent]): (GameState, List[GameEvent]) = {
		val gameCommand = gameCommands.head
		val (newGameState, events) = processCommand(gameCommand, gameState, List())

		if (gameCommands.tail.isEmpty) {
			(newGameState, events)
		} else {
			processCommands(NonEmptyList(gameCommands.tail.head, gameCommands.tail.tail), newGameState, events)
		}
	}

	def step(commands: List[GameCommand], gameState: GameState): (GameState, List[GameEvent]) = {
		val (processedCommandsGameState, processedGameEvents) = if (commands.isEmpty) {
			(gameState, List())
		} else {
			processCommands(NonEmptyList(commands.head, commands.tail), gameState, List())
		}
		(processedCommandsGameState.copy(step = processedCommandsGameState.step + 1), processedGameEvents)
	}

	def main(args: Array[String]): Unit = {
		val gameState = GameState()

		val commands = JoinGame("Bobby") :: JoinGame("Someone") :: Nil
		println(step(commands, gameState))
	}

}
