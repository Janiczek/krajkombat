# TODOs

- [x] main menu

- [x] story

- [x] visuals: you in your office (Kenney?)
- [x] game over visuals
- [x] game win visuals
- [x] game draw visuals

- [x] game loop of monthly decision making time
- [ ] game loop: showing new urgencies after some decisions
- [ ] game loop: urgency fail consequences

- [x] remaining months counter (starts at 12)
- [x] resource: AP (starts at default APmo)
- [x] resource: APmo (starts at 100)
- [x] resource: BBV (starts at 0)
- [x] resource: GREF (starts at 1.0)
- [x] resource: BREF (starts at 0.8)
- [x] upgrades (boolean list / set of ADTs)
- [ ] urgency list (+ their deadlines / months remaining before they blow up)
- [ ] available upgrades list (and after how many months they disappear - you've had your chance)

- [ ] upgrade: sabotage a competing region (take half their BBV)

- [x] other regions' resources (same types as yours)

- [x] random list of investments for this month
- [x] random list of preventions for this month
- [ ] random list of urgencies (and when they're revealed in this month)
- [ ] random list of president bribe opportunities (chance proportional to BBV or relative region ranking)
- [x] random list of random events for this month
- [ ] random list of available upgrades
      - [ ] unlocked via random events
      - [ ] unlocked via investments
      - [ ] unlocked via the president (chance is proportional to BBV or relative region ranking)

- [x] other regions' random decisions

- [x] game end: victory
- [x] game end: game over

- [ ] music
- [ ] sound fx
- [ ] jrpg style graphics?

- [ ] others: use upgrades too
- [x] upgrades: disable buy button if can't buy


- [ ] job/... market with other regions?

* random eventy:
  * +AP
  * -AP
  * +AP/mo
  * -AP/mo
  * +BBV (BrankyBodyVteřiny)
  * -BBV


* micromanagement: -AP, -AP/mo, +GREF (zaručí okamžitý úspěch akce / vyřeší problém, ale za cenu tvého času)

* time limit: 12 měsíců, ultimátní kolo KrajKombat ligy a výsledek podle BBV



# Creation of resources
- [x] +AP: when advancing month
- [ ] +AP: in some random events
- [x] -AP: in investments
- [x] +APPerMonth: in investments
- [x] -APPerMonth: in investmentsLongTerm
- [x] +GREF: in investments
- [ ] -GREF
- [ ] +BREF
- [ ] -BREF
- [ ] +BBV
- [ ] -BBV
- [ ] +BBVPerMonth
- [ ] -BBVPerMonth

# Usage of resources



        Urgency ->
            Random.uniform
                [ sub 5 10 APPerMonth, subF 0.01 0.03 BREF ]
                [ [ sub 10 20 AP, subF 0.01 0.02 BREF ]
                ]
