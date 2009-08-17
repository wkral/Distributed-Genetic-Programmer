(defun getGrammar ()
  '((start ((
"
#include <math.h>

#include \"q2bot.h\"
#include \"gpbot.h\"

#include \"gamestate.h\"
#include \"ammo.h\"
#include \"armor.h\"
#include \"health.h\"
#include \"player.h\"
#include \"powerup.h\"
#include \"projectile.h\"
#include \"weapon.h\"

struct BotThinkFunc* botRun(struct BotState* state) {
	
	static int timeStart = qbTickCount();
	
	gamestate_t* gs = &state->gs;
	player_t* self = state->self;
	int fire = 0;
	float time = (float)(qbTickCount() - timeStart) / 1000.0; 

	qbLog(\"Polling gamestate...\");	
	qbGetGameState(gs);

	// Parse the gamestate into our DOM
	Gamestate& gameState = Gamestate::instance();
	gameState.update( *gs );
	
	qbLog(\"Invoking bot thinkfunc...\");

	self->angles[0] = " float ";
	self->angles[1] = " float ";
	self->angles[2] = 0.0;

	self->velocity[0] = " float ";
	self->velocity[1] = " float ";
	self->velocity[2] = " float ";

	fire = " int ";

	botMovementOrder(gs, self->angles, self->velocity, fire);
	return NULL;
}
")))
    (float (("0.0") ("100.0") ("200.0") ("300.0") ("400.0") ("500.0") ("3.14159") ("10.0") ("9.0") ("8.0") ("7.0") ("6.0") ("5.0") ("4.0") ("3.0") ("2.0") ("1.0")
	    ("sin(" float ")") ("cos(" float ")") ("tan(" float ")")
	    ( float " + " float ) (float " - " float) (float " * " float) 
	    ("self->angles[0]") ("self->angles[1]") ("self->angles[2]") 
	    ("self->velocity[0]") ("self->velocity[1]") ("self->velocity[2]") 	    
	    ("time")
	    ("DIV(" float "," float ")") ))
    (int (("10") ("9") ("8") ("7") ("6") ("5") ("4") ("3") ("2") ("1") ("0") (int " + " int) (int " - " int) ("DIV(" int "," int")") (int " * " int) ("!" int)
    	("time") ))))
