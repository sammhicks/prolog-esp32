
servo_frequency(50).
servo_center(4824).
servo_half_range(3041).

servo_pin(left, 15).
servo_pin(right, 22).

channel(left, 0).
channel(right, 1).

forwards_direction(left, 1).
forwards_direction(right, -1).


steering_range(4000).


forwards_speed(8).
turn_speed(24).


setup :-
	setup(left),
	setup(right).


setup(Direction) :-
	servo_frequency(Frequency),
	servo_pin(Direction, Pin),
	channel(Direction, Channel),
	!,
	configure_channel(Channel, Frequency),
	analog_output(Pin, Channel).


stop :-
	stop(left),
	stop(right),
	delay(100).


stop(Direction) :-
	set_speed(Direction, 0).


set_speed(Direction, Offset) :-
	channel(Direction, Channel),
	forwards_direction(Direction, Sign),
	!,
	servo_center(Center),
	servo_half_range(Range),
	analog_write(Channel, Center + (Sign * Offset * Range) // 100).


steer(Steering) :-
	Steering >= 0,
	!,
	steering_range(Range),
	forwards_speed(Forwards_Speed),
	turn_speed(Turn_Speed),
	set_speed(left, min(Forwards_Speed + (Turn_Speed * min(Steering, Range)) // Range, 100)),
	set_speed(right, Forwards_Speed).

steer(Steering) :-
	Steering < 0,
	steering_range(Range),
	forwards_speed(Forwards_Speed),
	turn_speed(Turn_Speed),
	set_speed(left, Forwards_Speed),
	set_speed(right, min(Forwards_Speed + (Turn_Speed * min(-Steering, Range)) // Range, 100)).


raw_sensor(FL, L, CL, CR, R, FR) :-
	line_sensor(12, 14, 27, 26, 25, 33, FL, L, CL, CR, R, FR).


sensor(Position, Weight_Sum) :-
	raw_sensor(FL, L, CL, CR, R, FR),
	calculate_position(FL, L, CL, CR, R, FR, Position, Weight_Sum).


calculate_position(FL, L, CL, CR, R, FR, Position, Weight_Sum) :-
	Position_Sum = (CR - CL) + 3 * (R - L) + 5 * (FR - FL),
	Weight_Sum is FL + L + CL + CR + R + FR,
	Position is 1000 * Position_Sum // Weight_Sum.


follow_line :-
	sensor(Position, Weight_Sum),
	weight_sum_valid(Weight_Sum),
	!,
	millis(Time),
	follow_line_loop(Position, Time),
	stop.

follow_line :-
	stop.


follow_line_loop(Previous_Position, Previous_Time) :-
	sensor(Position, Weight_Sum),
	weight_sum_valid(Weight_Sum),
	!,
	millis(Time),
	Velocity is (Position - Previous_Position) // (Time - Previous_Time),
	Steering is Position + 500 * Velocity,
	steer(Steering),
	follow_line_loop(Position, Time).


follow_line_loop(_, _).


weight_sum_valid(Weight_Sum) :-
	Weight_Sum > 2000,
	Weight_Sum < 20000.
