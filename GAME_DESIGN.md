# CamlCast — Game Design Document

Status: vertical-slice design  
Genre: first-person psychological roguelike  
Platform: desktop, keyboard and mouse  
Working premise: an impossible house can be explored safely only by learning how
it warns you

This document has two deliberately separate parts:

1. **Game design** describes the player's experience, rules, progression, and
   intended decisions without prescribing code.
2. **Technical requirements** lists the engine and game-system capabilities
   needed to build that design.

---

# Part I — Game Design

## 1. Vision

The player explores an endless house assembled from rooms joined in ways that do
not form a possible floor plan. Some rooms are safe. Others kill the player the
moment they cross the threshold.

Every lethal room gives a warning that can be observed from the room before it.
The player opens a door, remains on the safe side, watches, and decides whether
to enter. They can deliberately record visible signs in a journal, but the game
does not explain what those signs mean. Death reveals a new *kind of danger*,
and the player must decide which previously observed sign predicted it.

Exploration is one-way. After the player crosses a doorway, it closes and locks
behind them. When they choose to end the expedition, they break the seal on the
most recent door. Every door on the route home unlocks, the house extinguishes
its ambient light, and the player must follow their own chalk marks back to the
entrance before their emergency lamp fails.

The run therefore has two distinct rhythms:

- **Going in:** patient observation, deduction, and irreversible choices.
- **Coming back:** urgent navigation through known space under failing light.

## 2. Design pillars

### Knowledge is the main progression

Death can reveal a new danger, but it never automatically reveals the warning
that predicted it. The player's journal contains their own theories, including
wrong ones. Later runs become more successful because the player understands the
house better, not because a character stat makes danger irrelevant.

### Every lethal choice is readable

A dangerous room may be unfamiliar, ambiguous, or accompanied by a harmless
decoy, but it must never be arbitrary. Its lethal sign is visible from outside
the room, through the same portal the player would cross.

### Going deeper must always be voluntary

The player can begin extraction from the locked door behind them at any point.
Every additional room increases the score at risk and may expose an unknown
danger. A safe return banks the run; death loses everything except knowledge.

### Navigation is physical

There is no automap, breadcrumb interface, or highlighted path home. The player
places chalk directly on walls and door frames. Those marks become especially
important when the house goes dark.

### The house is the threat

The vertical slice has no conventional enemies, weapons, combat, hunger, or
health bar. Dangerous architecture, incomplete knowledge, one-way exploration,
and the return through darkness create the tension.

## 3. The run

### 3.1 The safe room

Every run begins in a small, authored room outside the procedural house. It is
always safe and contains:

- the journal;
- the chalk supply and available symbols;
- the upgrade cabinet;
- the expedition's score and best-run records;
- one sealed doorway into a new generated house.

The starting room never counts toward the run score.

### 3.2 Outward exploration

The outward loop is:

1. Approach a closed door.
2. Open it without crossing the threshold.
3. Observe the room beyond.
4. Click any behavior worth recording.
5. Consult or update the journal if needed.
6. Decide whether the room is safe.
7. Cross, or close the door and inspect another route.

Crossing a safe threshold adds the room to the run if it has not been visited
before. The crossed door then closes and locks from both sides. The player
cannot return through it while the expedition remains in its outward phase.

The room just entered becomes the new frontier. Its locked entrance is also the
place from which extraction can be started.

Entering a dangerous room immediately ends the expedition. A short,
danger-specific death presentation identifies what happened, but not which
visual sign warned of it.

### 3.3 Committing to extraction

To begin the return, the player faces the locked entrance door behind them and
holds the interact action for 1.5 seconds. This breaks the survey seal and is
irreversible.

Commitment has five effects:

1. Every doorway in the exact traversal history becomes closed but unlocked.
2. Doors opened for inspection but not crossed are closed.
3. The starting safe-room door unlocks.
4. The house's ambient illumination fades over three seconds.
5. A 90-second emergency lamp becomes the player's remaining visibility.

Because outward doors prevent backtracking, extraction can never be postponed
until the player is already standing beside the entrance.

### 3.4 Returning

During extraction:

- doors no longer lock after crossing;
- previously safe rooms remain safe;
- new rooms may still be entered, but they add no score and retain their danger;
- chalk marks glow faintly;
- the journal remains available, but opening it does not pause the lamp;
- losing application focus pauses the game and lamp.

The route home is the reverse of the recorded doorway history, even if portals
formed loops or returned the player to a room from an impossible direction.
Nothing in the interface displays that route.

Re-entering the safe room banks the expedition. Reaching complete darkness or
crossing into a lethal room loses it.

## 4. Safe and dangerous rooms

### 4.1 Stable vocabulary

A sign has one meaning across every room type, seed, and run. If rising ash is
lethal once, rising ash is always lethal. Room shape alone does not change its
meaning.

Dangerous rooms contain exactly one lethal sign. At greater depths they may
also contain a harmless sign as a decoy. Safe rooms contain no lethal signs but
may contain harmless behaviors.

The initial depths introduce signs gradually:

- **Depths 1–3:** at most one behavior is visible in a room.
- **Depths 4–7:** the second danger joins the vocabulary; harmless signs become
  more common.
- **Depth 8 onward:** all three dangers may appear; dangerous rooms may pair
  their lethal sign with one harmless decoy.

### 4.2 Initial lethal signs

| Visible sign | Danger revealed after death | Required presentation |
| --- | --- | --- |
| Ash motes rise toward the ceiling | Suffocation | The room fills the player's view with ash and sound drops away |
| The ceiling slowly contracts and releases | Compression | The ceiling descends sharply as the view collapses |
| The floor pattern flows inward toward one point | Engulfment | The floor pulls the view down into darkness |

These signs must animate slowly enough to be missed by a careless glance and
reliably enough to be found by a player who watches for several seconds.

### 4.3 Initial harmless signs

| Visible sign | Meaning |
| --- | --- |
| Dust falls normally toward the floor | Harmless |
| Wall brightness pulses while the geometry remains still | Harmless |
| A floor ripple travels outward from its centre | Harmless |

Harmless signs are collectible observations. The journal does not label them
as harmless, and the player may incorrectly associate one with a death.

### 4.4 Generation fairness

The generated graph must satisfy these rules:

- The entrance and safe room are always safe.
- Every safe frontier region has at least one fresh safe continuation.
- A safe room with only its entrance and no onward doorway is allowed only as
  an optional dead-end choice, never as the sole continuation of the safe spine.
- Dangerous rooms are terminal. Nothing beyond them needs to be generated.
- The safe continuation is never marked by the interface.
- Dangerous and safe choices use the same doorway construction and room
  catalogue.
- The renderer must show every lethal sign needed for a decision from the safe
  side of its portal.

The existing impossible portal loops remain part of generation. Entering an
already visited safe room does not increase the score.

## 5. Journal

The journal is a persistent record shared by all runs. It has three collections.

### 5.1 Room types

When a room prototype is clearly visible through an open doorway for the first
time, its page is added automatically. A page contains its structural sketch,
common proportions, and observed instances. The initial room types are:

- corridor;
- chamber;
- landing;
- annex;
- rotunda;
- closet.

Room pages never state whether an instance is safe.

### 5.2 Collected signs

Signs are not collected merely because they were rendered. The player must:

1. remain outside the target room;
2. look directly at the behavior while it is visible;
3. left-click to record it.

A successful observation adds a descriptive sign card such as “ash rises” or
“floor draws inward.” Repeated observations of the same sign do not create
duplicates. Sign cards survive every kind of run ending.

### 5.3 Discovered dangers

A danger card is created automatically the first time that danger kills the
player. It describes the result—suffocation, compression, or engulfment—but not
the warning sign.

Signs and dangers begin unconnected. In the journal the player assigns one sign
card to one danger card. A sign and a danger can each participate in only one
link. Links can be changed at any time.

The game:

- stores incorrect theories;
- never validates a link;
- never supplies an automatic danger warning from a link;
- may repeat the player's own annotation when they inspect a known sign.

If the player dies without collecting the relevant sign, the danger card remains
unmatched until they observe that sign in a later run.

## 6. Chalk and navigation

Chalk is placed in the world, not in the journal.

- The player selects from their unlocked symbols.
- Pressing the chalk action places the selected symbol on the targeted nearby
  wall or door jamb.
- Every placement consumes one stroke.
- Marks are visible only from the side on which they were drawn.
- Marks never move, lie, disappear, or duplicate during the vertical slice.
- During extraction, their material becomes faintly phosphorescent.

The initial loadout has eight strokes and two symbols:

- arrow;
- cross.

The player may mark every return door, establish their own numbering language,
reserve chalk for confusing junctions, or use no marks and rely on memory.

## 7. Scoring and progression

### 7.1 Run score

The run score is the number of distinct safe procedural rooms entered before
extraction begins.

- Starting room: 0 points.
- First safe entry into a room: 1 point.
- Revisiting a room through a loop: 0 points.
- Looking into a room without crossing: 0 points.
- Entering a dangerous room: 0 points for that room.
- Entering new rooms during extraction: 0 points.

Maximum generated depth is shown as a secondary statistic but does not modify
the score.

### 7.2 Successful extraction

A successful return awards:

- the run score to best-run and lifetime records;
- an equal number of spendable survey credits.

Spending survey credits never lowers recorded scores.

### 7.3 Death

Death awards no score and no survey credits. The following knowledge persists:

- room-type pages;
- deliberately collected signs;
- danger cards earned from deaths;
- the player's sign-to-danger links;
- prior purchases and unused account-level credits.

### 7.4 Initial upgrades

| Upgrade | Cost | Effect |
| --- | ---: | --- |
| Chalk pouch I | 8 credits | Capacity increases from 8 to 12 |
| Chalk pouch II | 20 credits | Capacity increases from 12 to 16 |
| Chalk pouch III | 40 credits | Capacity increases from 16 to 20 |
| Circle stencil | 10 credits | Unlocks the circle symbol |
| Tally stencil | 25 credits | Unlocks the tally symbol |

Only chalk progression is required for the first vertical slice. Lamp duration,
movement, door speed, and danger readability are not upgradeable.

## 8. Controls

| Input | Action |
| --- | --- |
| `W`, `A`, `S`, `D` | Move |
| Mouse | Look |
| `E` | Open or close an unlocked targeted door |
| Hold `E` on current locked back door | Commit to extraction |
| Left click | Collect the visible sign under the centre of the view |
| `C` | Place the selected chalk symbol |
| Number keys | Select an unlocked chalk symbol |
| `Tab` | Open or close the journal |
| `F11` | Toggle fullscreen |
| `Esc` | Close the journal or quit from normal play |

The journal releases relative mouse capture so signs can be linked to dangers
with direct pointer interaction. Closing it restores first-person mouse look.

## 9. Presentation and feedback

The normal house keeps its restrained ash-grey surfaces, close fog, and
sourceless light. Danger signs rely primarily on motion and direction rather
than bright colors or warning icons.

Important sounds include:

- door opening, closing, locking, and unlocking;
- chalk scraping;
- distinct low sounds for each environmental behavior;
- the survey seal breaking;
- the house's ambient sound disappearing at extraction;
- lamp instability as time runs out;
- a unique sound for each death.

The return phase should be immediately recognizable without a large HUD. The
visual transition, unlocked-door sounds propagating through the house, glowing
chalk, and changing lamp should communicate the rule. A small lamp indicator
may show remaining time without displaying a precise countdown.

Text and imagery must be original. The game may take inspiration from impossible
architecture, unreliable documents, and spatial horror without reproducing
characters, passages, or distinctive terminology from existing fiction.

## 10. Example expedition

1. The player leaves the safe room and enters a corridor. Its door locks behind
   them. They chalk an arrow on its jamb.
2. Two doors lead onward. Through the first, dust falls normally in a chamber.
   The player clicks the dust and collects its sign card.
3. Through the second, ash rises in an annex. The player does not yet know what
   this means and chooses the chamber.
4. Several rooms later, the player sees rising ash again and risks entering.
   They die from suffocation.
5. The journal now contains the “Suffocation” danger and the previously
   collected “Ash rises” sign, if the player clicked it. The player links the
   two manually.
6. On a later run, the player recognizes rising ash and avoids it. After twelve
   distinct safe rooms, they decide the score is worth protecting.
7. They hold the locked back door, break the seal, and hear the route home
   unlock while the house goes dark.
8. They follow their arrows and crosses. A poorly marked landing costs time,
   but they reach the safe room before the lamp fails and bank twelve points and
   twelve credits.

## 11. Vertical-slice boundaries

Required:

- one safe-room hub;
- the existing six room prototypes;
- one-way outward doors and return unlocking;
- three lethal signs and three harmless signs;
- immediate room deaths;
- sign collection and manual journal matching;
- chalk placement and the listed upgrades;
- distinct-room scoring and extraction credits;
- return darkness and lamp failure;
- deterministic procedural generation and persistent saves.

Not required:

- combat or enemies;
- inventory beyond chalk;
- a route map;
- controller support;
- authored campaign chapters;
- daily challenges or online leaderboards;
- multiple playable characters;
- room hazards that change meaning between runs;
- moving or deceptive chalk;
- additional lamp or character upgrades.

---

# Part II — Required Technical Features

## 12. Architectural principle

The raycasting library should gain reusable capabilities—stateful doors,
interaction traces, dynamic room replacement, overlays—not game-specific
concepts such as “danger,” “journal,” “chalk currency,” or “extraction.”

House rules and content remain in `game/`; reusable rendering, geometry, input,
and loop support remain in `lib/`; the generated materials and images remain in
`assets/`.

## 13. Engine feature requirements

### 13.1 Stateful doors

**Current limitation:** a threshold either carries a door material or it does
not. A door is visually closed but does not block movement; walking into it
crosses the portal.

**Required capability:**

- Represent a door with its material and `Open`, `Closed`, or `Locked` state.
- Render open doors as portals and closed or locked doors as leaves.
- Block movement through closed and locked doors.
- Change a linked door atomically on both sides.
- Distinguish “closed but interactable” from “locked.”
- Preserve equal doorway dimensions, twin indices, and transforms.

**Game dependency:** inspection from safety, one-way outward exploration, and
return-route unlocking.

**Compatibility:** keep thresholds without doors valid for the engine demo and
other levels.

**Tests:** rendering-state selection through pure helpers, collision in all
states, synchronized twins, repeated state changes, and `World.check`
invariants.

### 13.2 Doorway traversal traces

**Current limitation:** movement returns only the final player pose. One
axis-resolved frame can cross more than one doorway, and the game needs every
crossing in order.

**Required capability:**

- Add a movement result containing the final player and an ordered list of
  crossings.
- Each crossing records source room, source threshold, destination room,
  destination threshold, and applied transform.
- Retain the existing `Player.walk` behavior as a wrapper returning only the
  final pose.

**Game dependency:** locking the correct door, counting distinct rooms, and
building an exact reversible traversal stack through loops.

**Tests:** no crossing, one crossing, two crossings in one frame, loop
re-entry, and crossing order.

### 13.3 Safe room replacement

**Current limitation:** growing a world can append a room or append a doorway,
but there is no public operation for changing a room's walls, decals, planes, or
sprites while retaining its existing links.

**Required capability:**

- Replace a room while preserving the count, order, name, endpoints, height,
  and portal relationship of every existing threshold.
- Permit changes to wall decals, fittings, floor and ceiling surfaces, sprites,
  and door state.
- Reject any replacement that invalidates a portal transform or twin index.

**Game dependency:** animated environmental signs, chalk decals, and extraction
lighting variants.

**Tests:** valid visual replacement, moved threshold rejection, changed
threshold order rejection, and portal integrity after replacement.

### 13.4 Vertically positioned sprites

**Current limitation:** every sprite stands directly on the room's floor.

**Required capability:**

- Give sprites a base height above the floor.
- Project, clip, fog, depth-test, and portal-mask elevated sprites exactly as
  floor-standing sprites are handled.
- Allow the game to select among precomputed animation frames without generating
  images during the render loop.

**Game dependency:** rising and falling dust particles and other collectible
visual signs.

**Tests:** floor-level compatibility, elevated projection, portal clipping,
wall occlusion, and sloped-floor placement.

### 13.5 Dynamic wall decals and targeting

**Current limitation:** wall decals are authored with the room and are not
designed for runtime placement.

**Required capability:**

- Ray-select the nearest visible wall or doorway jamb in the centre of view.
- Convert the hit into stable wall-local placement coordinates.
- Add a decal through safe room replacement.
- Preserve decals across later room rebuilds.

**Game dependency:** manual chalk symbols.

**Tests:** nearest-hit selection, placement coordinates, side specificity,
portal occlusion, persistence, and chalk-capacity rejection.

### 13.6 Portal-aware sign targeting

**Current limitation:** ray queries operate on one room at a time, while signs
must be clicked in the directly adjacent room without entering it.

**Required capability:**

- Trace the centre view through one open portal.
- Transform the ray into the adjacent room.
- Reject signs behind a closed door, wall, or nearer opaque object.
- Return a stable game-owned sign identifier when a visible target is clicked.

**Game dependency:** deliberate sign collection.

**Tests:** open and closed portal cases, occlusion, wrong viewing angle,
inactive animation phase, repeated collection, and signs in the current room
being ineligible.

### 13.7 Action-oriented input

**Current limitation:** input exposes movement plus quit and fullscreen only.

**Required capability:**

- Add edge-triggered actions for interact, primary click, chalk, chalk-symbol
  selection, journal, purchase, and restart.
- Distinguish a press from a held interaction.
- Toggle relative mouse mode when entering and leaving pointer-driven journal
  screens.

**Game dependency:** every non-movement mechanic.

**Tests:** action edge detection and hold duration in pure input-state helpers
where SDL itself cannot be tested headlessly.

### 13.8 Generic state-driven loop

**Current limitation:** the engine loop owns only a `World.t` and `Player.t`,
with a callback that can grow the world after a room change.

**Required capability:**

- Run an arbitrary game state through callbacks for simulation, world/player
  view, overlays, and termination.
- Deliver frame duration, motion, and actions to the game update.
- Preserve the current fixed-world `Engine.run` API as a compatibility wrapper.
- Pause elapsed game time when application focus is lost.

**Game dependency:** door state, phases, deaths, lamp, journal, economy, and
safe-room transitions.

**Tests:** legacy step compatibility, phase transitions, focus pause, and
deterministic updates from scripted input.

### 13.9 Framebuffer overlays

**Current limitation:** the renderer draws and immediately presents the 3D
frame.

**Required capability:**

- Allow an overlay callback after world rendering and before framebuffer upload.
- Supply clipped rectangles, lines, images, and a compact bitmap font.
- Support full-screen journal, death, shop, and results views as well as a
  minimal lamp indicator.

**Game dependency:** all two-dimensional interfaces.

**Tests:** pure layout calculations, clipping, text bounds, and overlay ordering.
Visual snapshots may be added if a deterministic framebuffer harness is
introduced.

### 13.10 Dynamic atmosphere

**Current limitation:** the world's atmosphere is fixed for the rendered frame.

**Required capability:**

- Derive atmosphere values from the current game phase and lamp level.
- Fade ambient visibility over the three-second extraction transition.
- Reduce fog distance and brightness as the lamp approaches failure.
- Apply a deterministic low-light flicker that does not consume layout or
  hazard RNG.

**Game dependency:** the change from outward exploration to return pressure.

**Tests:** transition curves, boundary values, lamp exhaustion, and RNG
isolation.

### 13.11 Audio

**Current limitation:** the engine has no audio layer.

**Required capability:**

- Initialize and release SDL audio with the same resource-safety guarantees as
  video resources.
- Play looping ambience and one-shot effects.
- Adjust lamp and environmental-behavior sound from game state.
- Use generated or original assets; do not depend on copyrighted source
  material.

**Game dependency:** readable environmental motion, door state, extraction
commitment, and death feedback.

**Tests:** pure mixer/state logic and graceful operation when audio
initialization fails. Manual testing covers device output.

## 14. Game-system requirements

### 14.1 Session state

The game session must own:

- phase: safe room, outward, dying, extraction, results;
- current world and player;
- layout generator;
- exact traversal stack;
- visited-safe-room set;
- run score and maximum depth;
- door states;
- lamp time;
- room hazard and sign metadata;
- collected chalk marks and remaining strokes;
- persistent journal and progression data.

Phase transitions must be explicit and testable. Rendering should read state but
never decide gameplay.

### 14.2 Hazard generation

Each room receives immutable per-run metadata:

- creation depth;
- safe or dangerous;
- lethal sign, if dangerous;
- harmless signs;
- animation phase and target placement.

Generation uses a dedicated hazard RNG derived from the run seed. It must never
consume the layout RNG, so the same seed retains the same room graph when
hazards, journal actions, or presentation change.

The generator must track and extend at least one safe spine. Dangerous rooms are
assembled for viewing but excluded from horizon expansion.

### 14.3 Door history and extraction

The outward crossing handler must:

1. receive the ordered crossing trace;
2. kill the player before normal control resumes if the destination is lethal;
3. count a safe destination only on first entry;
4. append the traversed link to the return stack;
5. close and lock that link.

Breaking the current back-door seal changes the phase only after the full hold
duration. It closes inspected doors, unlocks all stack links, and starts the
lamp exactly once.

Entering the safe room during extraction banks the run. Entering it by any
unexpected portal must not bypass the designated entrance or extraction rule.

### 14.4 Journal model

Persistent journal data must distinguish:

- automatically discovered room types;
- deliberately collected signs;
- dangers learned through death;
- player-authored one-to-one links.

The data model must permit incorrect links without storing a hidden
“validated” state in the user-facing journal. The simulation may know the true
mapping, but no UI function may expose it.

### 14.5 Progression and save data

Persist:

- journal collections and links;
- best score;
- lifetime banked rooms;
- deepest successful and unsuccessful run;
- survey-credit balance;
- chalk capacity tier;
- unlocked chalk symbols.

Use a versioned save format in the platform's user-data directory. Writes must
use a temporary file and atomic rename. A corrupt or newer unsupported file must
not crash the game; preserve it and start with safe defaults.

### 14.6 Safe-room interface

The safe room provides physical access to the journal and upgrade cabinet, but
the first slice may present those interactions as overlays once activated.

Purchases must:

- require sufficient credits;
- apply once;
- persist immediately;
- never reduce score records;
- define the next run's chalk loadout.

## 15. Testing and acceptance

### Automated tests

- Preserve all existing engine, renderer-helper, world, movement, and generator
  tests.
- Test every new state transition with deterministic scripted input.
- Generate at least 100 seeded houses and assert:
  - the safe spine continues;
  - dangerous rooms are never mandatory;
  - dangerous rooms are not expanded;
  - every lethal sign is visible from its parent portal;
  - every committed traversal stack can be reversed;
  - every mutated world passes `World.check`.
- Simulate deaths and extraction to verify exact score, credit, and knowledge
  persistence.
- Verify journal links have no effect on actual room safety or automatic UI.
- Verify layout generation is byte-for-byte equivalent when only hazard and
  presentation actions differ.

### Performance checks

- Simulate at least 1,000 generated rooms to measure the current append-only
  arrays.
- Target less than 20 ms for an ordinary frontier transition on the development
  machine.
- Precompute environmental images and animation frames; never generate textures
  in the per-pixel render loop.
- Only replace rooms currently contributing visible animation.

### Manual acceptance

- A careful player can see the decisive sign of every lethal room without
  crossing its threshold.
- The game never labels a room safe or dangerous.
- Opening a door never moves the player.
- Outward backtracking is impossible after a crossing.
- Extraction cannot be initiated beside the safe-room entrance.
- Chalk alone is sufficient to establish a reliable return route.
- A first successful extraction is achievable with eight chalk strokes.
- The outward phase feels methodical and the return phase feels urgent.
- After several deaths, journal knowledge produces noticeably deeper runs
  without changing the player's physical power.
