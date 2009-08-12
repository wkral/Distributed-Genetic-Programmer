#include <math.h>

#include "q2bot.h"
#include "gpbot.h"

#include "gamestate.h"
#include "ammo.h"
#include "armor.h"
#include "health.h"
#include "player.h"
#include "powerup.h"
#include "projectile.h"
#include "weapon.h"

struct BotThinkFunc* botRun(struct BotState* state) {
	
	static int timeStart = qbTickCount();
	
	gamestate_t* gs = &state->gs;
	player_t* self = state->self;
	int fire = 0;
	float time = (float)(qbTickCount() - timeStart) / 1000.0; 

	qbLog("Polling gamestate...");	
	qbGetGameState(gs);

	// Parse the gamestate into our DOM
	Gamestate& gameState = Gamestate::instance();
	gameState.update( *gs );
	
	qbLog("Invoking bot thinkfunc...");

	self->angles[0] = gpFLOAT;
	self->angles[1] = gpFLOAT;
	self->angles[2] = 0.0;

	self->velocity[0] = gpFLOAT;
	self->velocity[1] = gpFLOAT;
	self->velocity[2] = gpFLOAT;

	fire = gpINT;

	botMovementOrder(gs, self->angles, self->velocity, fire);
	return NULL;
}
