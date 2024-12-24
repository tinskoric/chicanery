# Chicanery

---
Authors: "Kenny Luong, Adam Sikorski, Tin Skoric, Issac Zheng"
---

------------------------------------------------------------------------

A game inspired by [Diplomacy](https://en.wikipedia.org/wiki/Diplomacy_(game) "Diplomacy (game) Wikipedia") but with its own identity.

## Overview

Chicanery is a multiplayer strategy game with simultaneous turns following the turn/phase format of Diplomacy and using traditional Diplomacy unit combat mechanics. For a thorough understanding of that game, see the online multiplayer implementation linked here: [webDiplomacy](https://webdiplomacy.net/intro.php "Intro to webDiplomacy"). Chicanery notably diverges from Diplomacy in several aspects, but aims to maintain the same core theme: emergent player interactions via negotiation (backstabbing your friends and starting fist-fights over a tabletop game from the '50s). Your fundamental objective remains to dominate the map by outwitting your friends.

## Mechanics

Chicanery introduces a series of new mechanics, to support player interaction, but chief among them is a unique implementation of voice-chat for negotiations. Chicanery uses private voice-chat rooms for negotiations between players, rather than messaging (the latter is typical for online diplomacy implementations), although messaging is available for players without microphones *within the room*. The rooms prevent players from being able to juggle multiple negotiations simultaneously, and instead force them to *pick* who they want to focus their energies on in the limited time they have per phase. Additionally, while the rooms are "private" from other players, it is not unfair to assume that if another player is not in your room talking with you, it's because they are talking to someone else. Moreover, there are ways for other players to gather more information on these "private" rooms and attempt to snoop.

Chicanery further introduces a series of mechanics that can be broadly categorized as relating to player **unit-management**, **economy**, and **research** in game. Click any of the below layers to expand for an explain-er on the eponymous group of mechanics!

<details>

  <summary>Units</summary>

This note is short as the combat mechanics of units remain the same—enjoy your attacks, holds, supports, and convoy orders—rather, units are slightly altered in how they recover during retreat phases. After each movement phase, units must be *replenished* before being usable in combat for the next phase. The below table details the scenarios or units in a given movement phase, and their subsequent replenishment need in the following retreat phase:

+------------------------------------------------------------------------------------------------------------------+--------------------------------------------+
| Action                                                                                                           | Replenishment Need                         |
+==================================================================================================================+============================================+
| Unit did not engage in combat with an **enemy**[^1] unit (either defensive or offensive!) in the movement phase. | No need.                                   |
+------------------------------------------------------------------------------------------------------------------+--------------------------------------------+
| Unit engaged in **defensive** combat and was **successful**.                                                     | Base of 20% replenishment need.            |
+------------------------------------------------------------------------------------------------------------------+--------------------------------------------+
| Unit engaged in **defensive** combat and **failed** (must retreat/burn in retreat phase).                        | If retreated: base 70% replenishment need. |
|                                                                                                                  |                                            |
|                                                                                                                  | If burned: no need.                        |
+------------------------------------------------------------------------------------------------------------------+--------------------------------------------+
| Unit engaged in **offensive** combat and was **successful**.                                                     | No need[^2].                               |
+------------------------------------------------------------------------------------------------------------------+--------------------------------------------+
| Unit engaged in **offensive** combat and **failed** (didn't take territory).                                     | Base of 50% replenishment need.            |
+------------------------------------------------------------------------------------------------------------------+--------------------------------------------+

: Unit Replenishment Chart

As can be seen, losing a battle over a tile (province) where you are defending is very costly, particularly at the start. **If a unit is not replenished, it will be unusable for combat actions, including for support orders**. An un-replenished unit will not be able to take any orders—no attacks, holds, or supports—*except convoys that would bring it to friendly territory*[^3](cannot be used in convoys for attacking). An un-replenished unit will not hold if attacked, no matter if it is supported or not, and rather will immediately retreat[^4]. Replenishment costs *income* and the costs can be altered by *research*, which segues us nicely into the next sections.

</details>

[^1]: If you bounce your own units, and no one else tries to attack the area you bounced, then that counts as not having engaged in combat with those units—so long as those units weren't attacked otherwise.

[^2]: Pillaging sure is great.

[^3]: Why you would to do this is... a question—but it is a permitted order.

[^4]: Choosing not to replenish a unit is NOT a death sentence for it. It will just keep retreating until you replenish it. It is perfectly viable—if you are okay with losing ground—to save money by not replenishing some units.

<details>

  <summary>Economy</summary>

In traditional (we're not talking about esoteric variants right now) Diplomacy, the player "economy" is extremely straightforward. There are a number of supply centers (centers hereafter) on the board based in cities. The number of centers you have defines how many units (referring to armies/fleets) you may build. In Chicanery, this relationship is abstracted.

In Chicanery, centers generate **income** (denoted, $I$, in included formulae) and **action points** (denoted AP hereafter) which serve dual functions. Income is used to *purchase* (and *replenish*, in the case of units) things like units, buildings, and "accoutrements"[^5] (discussed in the research subsection). AP are used to *order* units and accoutrements. Income and AP are earned and accrued differently across turns and phases, illustrating the subtle difference in their functions. See the below sections for details:

### Income

Income is a linear function of the number of centers controlled between **turns**, earned at the start of the Spring phase (the start of a new turn), and available to be spent on various uses across all phases. Income is calculated purely by multiplying a scalar by the number of centers you possess (and adding bonuses from research).

+----------------+---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Phase          | Description                                                                                                                                                                           |
+================+=======================================================================================================================================================================================+
| Spring ($\pm$) | Income from centers held after the previous Fall phase is earned and added to the rolling total. Income can be spent on research (like replenishment cost buffs!) and research items. |
+----------------+---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Summer ($-$)   | Income is spent on replenishing units.                                                                                                                                                |
+----------------+---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Fall ($-$)     | Income can be spent on research items.                                                                                                                                                |
+----------------+---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Winter ($-$)   | Income is spent on replenishing units.                                                                                                                                                |
+----------------+---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Build ($-$)    | Income is spent on purchasing new units and research items.                                                                                                                           |
+----------------+---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+

: Income by Phase

Lastly, **income can be saved between phases and turns**. You will not lose the income you do not spend, it will roll over from one turn to the next. You **can** send other players a portion (or all) of your income.

### Action Points (AP)

AP are a decreasing function of the number of centers controlled between **phases**, earned at the start of the Spring and Fall phases, and available to be spent for use across all phases. AP is calculated by adding a fixed base number for a given number of centers and an exponentially decreasing function of your total center-count. As your state grows, the relative AP that you get from each center is less and less, constraining you opportunities for orders in each turn.

+----------------+-------------------------------------------------------------------------------------------------------------------------------------------+
| Phase          | Description                                                                                                                               |
+================+===========================================================================================================================================+
| Spring ($\pm$) | AP from centers held after the previous Fall phase is earned. AP can be spent on issuing orders (attack, hold, support, convoy) to units. |
+----------------+-------------------------------------------------------------------------------------------------------------------------------------------+
| Summer ($-$)   | AP is spent on issuing retreat orders to units.                                                                                           |
+----------------+-------------------------------------------------------------------------------------------------------------------------------------------+
| Fall ($\pm$)   | AP from centers held after the Spring phase is earned. AP can be spent on issuing orders to units.                                        |
+----------------+-------------------------------------------------------------------------------------------------------------------------------------------+
| Winter ($-$)   | AP is spent on issuing retreat orders to units.                                                                                           |
+----------------+-------------------------------------------------------------------------------------------------------------------------------------------+
| Build ($-$)    | AP is spent on placing new units and researched buildings—ports, supply hubs, and forts.                                                  |
+----------------+-------------------------------------------------------------------------------------------------------------------------------------------+

: Action Points (AP) by Phase

If you gain/lose centers between the Spring and Fall phases, you will see the AP you earn in the latter phase altered, even if your opponent does not control that center for the whole turn.

> E.g., if you control a center at the outset of Spring `year-something` and someone else takes it in that turn, then in Fall `year-something`, your earned AP in the latter turn will not include AP that would've been earned from that center. Conversely, if you were the other player who gained it, your AP in the latter turn will include the income from that center.

Lastly, **AP can only be saved between phases**. You WILL lose the AP you do not spend between turns, it will **not** roll over from one year to the next. You **cannot** send other players your AP.

</details>

[^5]: Chose a fancy-sounding word but its really just a research tree that gives some buffs to economic management and some useful tools for deceit/strategy.

<details>

  <summary>Research</summary>

Last but not least is research. This mechanic integrates with the unit and economy changes, but also includes some fun gameplay elements. First there are plain buffs. These buffs center around reductions to the income cost of replenishment, some income bonuses for taking centers, and increases to the amount of AP earned for larger players. All items—buildings, accoutrements—must first be researched for a fixed cost of income. After being researched, individual buildings must be bought with income and placed with AP each time, and individual accoutrements must be bought with income (no AP needed for their use!). **Accoutrements are tools for subterfuge and deceit**. In the case of buffs, research functionally just means paying more for a better effect as the research for buffs is reset each turn. For buildings, research is a one-time cost to be able to build something—the research is not reset. For accoutrements, research is a one-time cost—the research is not reset—but accoutrements are one-time use, and must be bought for each use. Below are sections detailing research across the three defined groups:

### Buildings

There are only three buildings, they cost income to research (once), and then income purchase and AP to place, and they can only be placed during the build phase. Buildings act to make small adjustments to traditional Diplomacy mechanics. Although these buildings are *not* temporary, there is a limit to the number of buildings you can place according to the total number of provinces you command, and they can be destroyed or lost. If you exceed your limit, then you must delete buildings during the Build phase until you are within your limit.

| Type | Effect |
|----|----|
| Fort | Building a fort in a tile (province) simply adds a permanent +1 to defense in that tile. Think of it as having a second army that can take no orders but automatically follows a support-defend order on whatever *friendly* unit is occupying the tile. Unlike the other two buildings, **forts cost replenishment**—equal to the costs of a typical unit. If a defense for a tile is lost and an enemy takes control, the opposing player who has assumed control of the province must pay to replenish the fort. Unlike units, which will simply retreat if left un-replenished, if a fort is left un-replenished for two entire movement phases (Spring and Fall or Fall and Spring) after first needing replenishment, it will be destroyed[^6]. Forts *do not* earn income or AP, and they *do not* contribute to your total number of centers. Ports require replenishment (as detailed) and *are not* destroyed upon the loss of a tile—so long as they are replenished. |
| Port | Building a port in a tile (province) allows one army and one fleet (or two fleets and no army) to simultaneously occupy the same tile, by effectively creating a sub-tile for the fleet. Since the actual tile continues to hold only a single unit this still counts only as a +1 defense rather than a +2. If the main tile is lost, both units must retreat to their respective adjacent options. Each unit can still operate independently from the province (e.g., they can attack or support different things, **including each other**[^7]). Ports *do not* earn income or AP, and they *do not* contribute to your total number of centers. Ports *do not* require replenishment and *are not* destroyed upon the loss of a tile—ports are only destroyed if you have to destroy them to stay within your building limit. |
| Supply Hub | In classic Diplomacy, you can only build units in your starting centers... until you build a supply hub! Supply hubs allow you to build units in whichever tile (province) a supply hub is present in. Supply hubs *do not* earn income or AP, and they *do not* contribute to your total number of centers. Supply centers *do not* require replenishment, but **are immediately destroyed upon the loss of the tile**[^8]. |

: Research Information: Buildings

### Buffs

Buffs include reductions to the income cost of replenishment, some income bonuses for taking centers, and increases to the amount of AP earned for larger players. Buffs are TEMPORARY, and only last the duration of each turn, but activate automatically after research—within the same *phase*. These reductions are earned via research paths and stack. For some examples:

Reduction to Cost of Replenishment:

> Researching three buffs down this route will reduce all costs by 30%—at first only by 5%, then by 10% for the next time, and 15% after than. It is impossible to entirely eliminate all costs of replenishment, but these reductions result in defensive and offensive losses costing only 40% and 20% respectively, and all other cases costing **nothing**.

Bonuses for Taking Centers:

> Pillaging! Researching buffs down this route will give an income reward for taking centers that is increased per level of research down the route and stacks with previous levels. Researching three buffs down the route will give a bonus of +6% of total income earned from the start of a turn.
>
> > E.g., if you earned 100 income at the start of Spring `year-something`, and you purchased three buffs down this route, then when you capture any center in the same phase (Spring `year-something`) or the next phase (Fall `year-something`), you with earn 6 income immediately—in the same phase—for the capture of that center.

Increases to AP Earned:

> Makes the exponential decrease in AP earned per center flatter.

### Accoutrements

**These are the fun tools**. Accoutrements must be researched (once) using income, and then purchased for each use using income. Unlike buildings, these do not cost AP, and unlike buffs, they are not temporary to only the turn they were purchased in, but rather are one-time use upon purchase and remain available for use across phases and turns until they are used. Accoutrements range from snooping- to "counter-snooping"-related niceties. For example:

War Plans:

> Researching down this path and purchasing the associated accoutrement allows you to get hints on one specified order (*1* move can still be a notable informational advantage) in a tile (province) by another player at the time the accoutrement is used[^9]. The higher the level of research, the more accurate the hint—at lower levels, dummy moves will be by a probability chance, and/or the type of move may be reported, but not the destination.

False Flags:

> Researching down this path and purchasing the associated accoutrement allows you to *fake* hints served by the "War Plans" accoutremont for a specified tile, providing them as dummy moves. While War Plans requires selecting a tile, False Flags work across all tiles as a passive effect to counter War Plans for the phase in which the former is activated[^10]. This muddles the information war between players.

Government Insiders:

> Researching down this path and purchasing the associated accoutrement allows you to get estimates of other players incomes and AP.

Hi-Fi Audio:

> Researching down this path allows you to see more details about voice-chat rooms such as:
>
> 1.  The total number of rooms are a given time;
> 2.  The players currently in *any* call.
>
> No level of research will show exactly which player is in which call, but by knowing the details above and using some deduction it should be possible to decipher who is with who in what calls.

</details>

[^6]: To elaborate: if you beat someone in Spring `year-something` and take their tile, and that tile had a fort stationed therein, then if you do not replenish the fort **before** Fall of `year-something+1` (so after Fall of `year-something` and Spring of `year-something+1` , up to Summer of `year-something+1` at the latest) it will be destroyed and removed from the map along with the +1 defense in the tile.

[^7]: In this case, you can get a +2 if you perpetually have the unit in the port defending the unit in the main tile.

[^8]: If player A builds a supply hub in Build of `year-something` and player B takes it in the Spring of `year-something+1`, then the supply hub will be destroyed.

[^9]: Be careful, if you use it before the enter an order, you'll get nothing!

[^10]: That means that it is in your interest to activate this early in the phase you want to use it in!
