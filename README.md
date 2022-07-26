# pencil: the desmos-inspired programming language

example snake program:
```py
tick(500)
	{ alive: move!() }
settings
	rows = 15
	width = 10
	buttonSize = 0.6
util
	head = body[0]
	bw = width / rows
	newHead = head + direction
	gonnaEatApple = { newHead = apple }
	selfCollisions = sum([ { newHead = body[i] } for i = [0..length(body)] ])
colors
	bounds = rgb(0, 255, 0)
	snake = rgb(0, 100, 255) | [ rgb(0, 0, 255) for i = [0..length(body) - 1] ]
	apple = rgb(255, 0, 0)
	moveButton = rgb(64, 64, 64)
	directionIndicator = rgb(150, 0, 255)
	restartButton = rgb(255, 0, 0)
	youDied = rgb(0, 0, 0)
draw
	<snake> body * bw - (bw/2, bw/2)
	<directionIndicator> [ head, newHead ] * bw - (bw/2, bw/2)
	<apple> apple * bw - (bw/2, bw/2)
	<bounds> [ (0, 0), (width, 0), (width, width), (0, width), (0, 0) ]
gui
	<moveButton> rect(6 * buttonSize - 2, y, buttonSize, buttonSize, 5) -> setDir!(-1, 0)
	<moveButton> rect(6 * buttonSize - 2, y, buttonSize, buttonSize, 5) -> setDir!(0, 1)
	<moveButton> rect(6 * buttonSize - 2, y, buttonSize, buttonSize, 5) -> setDir!(1, 0)
	<moveButton> rect(6 * buttonSize - 2, y, buttonSize, buttonSize, 5) -> setDir!(0, -1)
actions
	move!() = {
		newHead.x <= 0 and newHead.x > rows or newHead.y <= 0 or newHead.y > rows or selfCollisions > 0: die!(),
		gonnaEatApple: (body -> newHead | body, genApple!()),
		(body -> newHead | [ body[i] for i = [0..length(body) - 1] ])
	}
	
	setDir!(x, y) = { alive: { length(body) = 1: setDirInternal!(x, y), x = -direction.x: (), y = -direction.y, setDirInternal!(x, y) } }
	setDirInternal!(x, y) = direction -> (x, y)
data
	alive = 0
	direction: (x, y) = (0, 0)
	body: [ (x, y) ] = [ (7, 8) ]
	apple: (x, y) = (11, 8)
```
