-- lesson10.2_robot.hs

-- robot :: ((String, Int, Int) -> t) -> t
robot :: (String, Int, Int) -> ((String,Int,Int) -> t) -> t
robot (name,attack,hp) = \message -> message (name,attack,hp)
-- The above could also be expressed as this:
-- robot (name,attack,hp) message = message (name,attack,hp)


-- helper functions for getters / accessors
name   (n,_,_) = n
attack (_,a,_) = a
hp     (_,_,h) = h

-- getters / accessors
getName   aRobot = aRobot name
getAttack aRobot = aRobot attack
getHP     aRobot = aRobot hp

-- -- An alternative to using the helper functions would be to use lambdas so:
-- getName   aRobot = aRobot (\(n,_,_) -> n)
-- getAttack aRobot = aRobot (\(_,a,_) -> a)
-- getHP     aRobot = aRobot (\(_,_,h) -> h)

-- setters
--                           +-> new robot "object"
--                           |      +-> get the passed robot "instance"
--                           |      |       +-> apply λ that extracts properties
--                           |      |       |
--                          \|/    \|/     \|/
setName   aRobot newName   = robot (aRobot (\(_,a,h) -> (newName,a,h)  ))
setAttack aRobot newAttack = robot (aRobot (\(n,_,h) -> (n,newAttack,h)))
setHP     aRobot newHP     = robot (aRobot (\(n,a,_) -> (n,a,newHP)    ))

-- -- A more verbose alternative to the lambda approach:
-- setName aRobot n = robot (n,a,h)
--   where a = getAttack aRobot
--         h = getHP aRobot
-- setAttack aRobot a = robot (n,a,h)
--   where n = getName aRobot
--         h = getHP aRobot
-- setHP aRobot h = robot (n,a,h)
--   where n = getName aRobot
--         a = getAttack aRobot

printRobot aRobot = 
  aRobot (\(n,a,h) -> n ++ " -- attack: " ++ show a ++ " -- hp: " ++ show h)
    
damage aRobot attackDamage = 
  robot (aRobot (\(n,a,h) -> (n,a,h-attackDamage)))

fight attackRobot defendRobot = damage defendRobot attack
  where attack = if getHP attackRobot > 10
                 then getAttack attackRobot
                 else 0

-- fresh robots
killerRobot = robot ("Kill3r", 25, 200)
gentleGiant = robot ("Mr. Friendly", 10, 300)

-- fight!
-- round 1
gentleGiantRound1 = killerRobot `fight` gentleGiant
killerRobotRound1 = gentleGiantRound1 `fight` killerRobot
-- round 2
gentleGiantRound2 = killerRobotRound1 `fight` gentleGiantRound1
killerRobotRound2 = gentleGiantRound2 `fight` killerRobotRound1
-- round 3
gentleGiantRound3 = killerRobotRound2 `fight` gentleGiantRound2
killerRobotRound3 = gentleGiantRound3 `fight` killerRobotRound2


-- new robots
fastRobot = robot ("speedy",15,40)
slowRobot = robot ("slowpoke",20,30)

-- round 1
slowRobotRound1 = fight fastRobot slowRobot
fastRobotRound1 = fight slowRobotRound1 fastRobot
-- round 2
slowRobotRound2 = fight fastRobotRound1 slowRobotRound1
fastRobotRound2 = fight slowRobotRound2 fastRobotRound1
-- round 3
slowRobotRound3 = fight fastRobotRound2 slowRobotRound2
fastRobotRound3 = fight slowRobotRound3 fastRobotRound2


-- 
robot1 = robot ("One",15,400)
robot2 = robot ("Two",20,300)
robot3 = robot ("Billy",35,315)
-- map getHP [robot1,robot2,robot3]

-- type Robot = ((String, Integer, Integer) -> a) -> a
-- What the actual fuck? I can't figure this out at all. I just get a
-- "cannot construct the infinite type" error
threeRoundFight ra rb = if getHP ra3 > getHP rb3 then ra3 else rb3
  where ra3 = fight rb0 (fight rb0 (fight rb0 ra0))
        rb3 = fight ra0 (fight ra0 (fight ra0 rb0))
        ra0 = robot (ra (\(na,aa,ha) -> (na,aa,ha)))
        rb0 = robot (rb (\(nb,ab,hb) -> (nb,ab,hb)))

threeRoundFight' x y = if za > zb then za else zb
  where za = (+) x y
        zb = (-) x y

ra0 = robot ("sam",10,10000)
rb0 = robot ("tex",5,50000)
ra1 = fight rb0 ra0
ra2 = fight rb1 ra1
ra3 = fight rb2 ra2
rb1 = fight ra1 rb0
rb2 = fight ra2 rb1
rb3 = fight ra3 rb2

ra3' = fight rb3 (fight rb2 (fight rb1 ra0))
rb3' = fight ra3 (fight ra2 (fight ra1 rb0))
