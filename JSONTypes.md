## Ejemplo de entrada de escena de exploración

```json
{
	"id": "FORESTMAP_1",
	"type": "exploration",
	"narrative": {
		"title": "Claro del Bosque",
		"text": "Te encuentras en un claro del bosque. Puedes explorar libremente."
	},
	"exploration": {
		"mapId": "forest_clearing",
		"gridSize": {
			"width": 10,
			"height": 10
		},
		"playerStartPos": {
			"x": 5,
			"y": 9
		},
		"interactables": [
			{
				"id": "old_tree",
				"pos": {"x": 2, "y": 3},
				"type": "object",
				"name": "Árbol Antiguo",
				"entryId": "TREE_EXAMINE"
			},
			{
				"id": "forest_path",
				"pos": {"x": 5, "y": 0},
				"type": "trigger",
				"name": "Sendero del Bosque",
				"entryId": "LEAVE_CLEARING"
			}
		]
	}
}
```
## Ejemplo de entrada de escena de combate

```json
{
	"id": "GOBLINBATTLE",
	"type": "combat",
	"narrative": {
		"title": "¡Emboscada de Goblins!",
		"text": "Tres goblins saltan de los arbustos, blandiendo armas oxidadas. ¡Prepárate para luchar!"
	},
	"combat": {
		"enemies": [
			{
				"id": "goblin_1",
				"name": "Goblin Guerrero",
				"stats": {
					"ac": 15,
					"hp": 7,
					"maxHP": 7,
					"speed": 30,
					"str": 8,
					"dex": 14,
					"con": 10,
					"int": 10,
					"wis": 8,
					"cha": 8
				},
				"attacks": [
					{
						"name": "Hacha de batalla",
						"bonus": 1,
						"damage": "1d6+1",
						"damageType": "slashing"
					}
				]
			},
			{
				"id": "goblin_2",
				"name": "Goblin Lanzador",
				"stats": {
					"ac": 15,
					"hp": 7,
					"maxHP": 7,
					"speed": 30,
					"str": 8,
					"dex": 14,
					"con": 10,
					"int": 10,
					"wis": 8,
					"cha": 8
				},
				"attacks": [
					{
						"name": "Lanza",
						"bonus": 4,
						"damage": "1d6+2",
						"damageType": "piercing"
					}
				]
			}
		],
		"onVictory": "DEADGOBLINS",
		"onDefeat": "OHWELL"
	}
}
```
## Ejemplos de efectos soportados

// Recibir daño
```json
{"type": "damage", "params": {"amount": 5}}
```

// Curar HP
```json
{"type": "heal", "params": {"amount": 10}}
```

// Ganar objeto
```json
{
	"type": "gainItem",
	"params": {
		"item": {
			"itemName": "Poción de Curación",
			"itemValue": 50,
			"itemType": "Potion"
		}
	}
}
```
