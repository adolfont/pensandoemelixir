--- 
mapr: true
size: 16:9
theme: default
paginate: true
backgroundColor: lightpink 
header: 'Learning Elixir and Erlang with Advent of Code and Exercism: advantages and challenges'
footer: 'Adolfo Neto (UTFFPR)'
---

# Learning Elixir and Erlang with Advent of Code and Exercism: advantages and challenges


## Adolfo Neto  [@adolfont on Twitter](http://twitter.com/adolfont)

[adolfont.github.io](http://adolfont.github.io)

![w:200 h:200](img/qr.png) 

![bg 100% right:20%](img/cbac2.png)


<!-- ![bg 100% right:10%](img/cbl2.png) -->



<!-- First slide --> 
---

# Presentation script

- Who am I?
- What is Advent of Code?
- What is Exercism?
- What is good/bad in AoC/Exercism?
- **How ​​to use AoC and/or Exercism to learn Erlang and/or Elixir?**

<!-- Script --> 

---


# Who am I?

- Professor at the [Federal University of Technology – Parana (UTFPR)](http://www.utfpr.edu.br/english/about-utfpr/facts-and-figures)

 ![h:250](img/utfpr.jpg) ![h:250](img/queijos.jpg) ![h:250](img/abraco.jpg)

---  

# Who am I? (part 2)

- Co-host of [Elixir em Foco (Elixir in Focus)](https://www.elixiremfoco.com/) podcast (in Brazilian Portuguese)
- Member of the [Education, Training, & Adoption Working Group of the Erlang Ecosystem Foundation](https://erlef.org/wg/education)
- [YouTube EN](https://www.youtube.com/c/ElixirErlangandtheBEAMwithAdolfoNeto), [YouTube PT](https://youtube.com/c/AdolfoNeto), [Instagram](http://instagram.com/adolfont), [Telegram](http://t.me/adolfont), [GitHub](http://github.com/adolfont), [Twitch](http://twitch.tv/adolfont), [TikTok](https://www.tiktok.com/@adolfont)...

<!-- Too many things... Open to work? --> 

---

# Which are the difficulties in learning Elixir and Erlang?

- Common programming pitfalls
- Erlang/Elixir pitfalls
  - Functional programming concepts
  - Syntax details
  - Not knowing the best practices of the communities
  - Concurrency/OTP (difficult by itself)
  
<!-- Pitfalls for all programming languages vs pitfalls specific to Elixir and/or Erlang --> 

![bg 100% right:10%](img/cat.png) 

---

## What helps? (in my opinion)

- Reading [books](https://dev.to/adolfont/the-elixir-community-owes-a-lot-to-pragmatic-programmers-glf) or well-structured materials ([Elixir School](https://elixirschool.com/en))
- Having a mentor
- Chatting with other people
- Participating in the community
	- [Elixir Forum](https://elixirforum.com/)
	- [Erlang Forums](https://erlangforums.com/)
	- \#MyElixirStatus, \#ElixirLang\, \#WeBEAMtogether, \#Erlang
	- [Elixir World](https://t.me/elixir_world)
- Listening to [podcasts](https://github.com/elixir-lang/elixir/wiki/Podcasts-and-Screencasts) 
- What else?

![bg 100% right:10%](img/cat2.png) 

---

## What helps? (in my opinion)

- Putting knowledge into practice - just a little bit above your current level

- Write code idiomatically, the way more experienced people in the community write

---

# Which is most used? I don't know, but...

"Have you ever solved an Advent of Code or an Exercism challenge?"


![w:908 h:293](img/sum.png)


![bg 100% right:10%](img/cat3.png) 

<!-- 
- [Twitter Poll (PT)](https://twitter.com/elixiremfoco/status/1510973616602722308)
- [Poll on Elixir Forum](https://elixirforum.com/t/have-you-ever-solved-an-advent-of-code-or-an-exercism-challenge/46985)
- [Poll on Erlang Forums](https://erlangforums.com/t/have-you-ever-solved-an-advent-of-code-or-an-exercism-challenge/1285)
- [Sum](https://docs.google.com/spreadsheets/d/1COt5PrCVjm7hnNTBWC27UI9FsVr_qYOkkocWNZE1T5I/edit?usp=sharing)

-->

---

# What is the Advent of Code?

- Website: [https://adventofcode.com/](https://adventofcode.com/)
- Wikipedia: [https://en.wikipedia.org/wiki/Advent_of_Code](https://en.wikipedia.org/wiki/Advent_of_Code)
- Creator: [Eric Wastl](http://was.tl/)

- Annual “event” that goes from December 1st to December 25th
  - It's a kind of competition
  - There are private leaderboards
  - What counts for the ranking is who finds the answer, usually a number, first
- So the time zone counts.

---

# Me in Advent of Code

- 2021: 1 to 10, 13 and 14 - total 12x2 = 24
- 2020: 1 to 9 and part 1 of day 10: total 19
- 2019: 1 and 2. Total: 4


Where I am saving my solutions as of 2021
https://github.com/adolfont/pensandoemelixir/tree/main/adventofcode/

![bg 100% right:10%](img/cat4.png) 

---

# Important!

- Relatively long description (631, 577, 1002)
- Pay close attention to the wording!
  - [Day 2 Part 1 2019](https://adventofcode.com/2019/day/2)
  - _"To do this, before running the program, replace position 1 with the value 12 and replace position 2 with the value 2."_
- Undisclosed difficulty
- Ad-hoc solutions

<!-- "Ad hoc is a Latin phrase meaning literally 'to this'. In English, it typically signifies a solution for a specific purpose, problem, or task rather than a generalized solution adaptable to collateral instances. (Compare with a priori." https://en.wikipedia.org/wiki/Ad_hoc ) -->

![bg 100% right:10%](img/cat5.png) 

---

![bg 50%](img/exercism1.png) 

---

![bg 90%](img/exercism2.png) 

---

# What is Exercism?

- Created by [Katrina Owen](https://archive.is/2MJ5F)
    - read [https://www.kytrinyx.com/exercism/](http://web.archive.org/web/20210509192904/http://www.kytrinyx.com/exercism/) !!!
    - and <https://en.wikipedia.org/wiki/Exercism>

- Exercises + free mentoring
- It gives you short descriptions (72, 96, 172)
- It provides you with tests for your programming language
- Current version: version 3

Access [https://exercism.org/]()https://exercism.org/

![bg 100% right:10%](img/cat6.png) 

---

# Mentoring example

Private link:
https://exercism.org/mentoring/discussions/ad416ef10f1e42b4b97445f2ee672f53


---

![bg 60%](img/mentoring.png) 


---

![bg 80%](img/mentoring2.png) 

---

![bg 65%](img/mentoring3.png) 

---

# José Valim and the Advent of Code

- [José Valim's Twitch: 2018 and 2021](https://www.twitch.tv/josevalim)

- [Sekun's YouTube playlist with shorter, edited versions of Valim's videos](https://www.youtube.com/playlist?list=PLNP8vc86_-SOV1ZEvX_q9BLYWL586zWnF)
  - [Support SEKUN](https://ko-fi.com/sekun)

---
  
![](img/sekun.png) 

---

# Good/bad in AoC

- What's good about AoC?
    - Community (Twitter, Elixir Forum and Erlang Forums)
- What is "bad" in AoC?
    - Weekends! _But [SpawnFest](https://spawnfest.org/) too :)_
    - December! _Spring/Summer in the Southern Hemisphere_
    - Some challenges require (a lot of) prior knowledge (programming competitions)
    - Is there a stimulus to write unreadable code?

---

# Good/bad in Exercism

- What is good about Exercism?
    - mentoring
    - tests
    - automatic feedback
- What is "bad" in Exercism?
    - the tests are already there
       - It is impossible to do TDD
    - v3 kind of discourages mentoring (debatable)

---

# What have I learned? 1/3

- Use of Enum (Valim) #aoc #elixir
	- _There's an Enum function for that!_
- Lots of pipes!

- Functions with the same name but different arity, one being public and the other private #erlang #exercism
- Various uses of [list comprehension](https://www.erlang.org/doc/programming_examples/list_comprehensions.html):

`[command(S) || S <- string:split(String, [$\n], all), S =/= ""].`

![bg 100% right:10%](img/cat.png) 

--- 
# What have I learned? 2/3

- [case](https://github.com/adolfont/pensandoemelixir/blob/main/adventofcode/2021/day02_2021/day02_2021_v2.erl) #erlang #aoc

![](img/case.png) 
- [read, understand and run code from others](https://github.com/adolfont/pensandoemelixir/blob/main/adventofcode/2021/day04_2021/danilagamma.erl) #erlang #aoc

![bg 100% right:10%](img/cat3.png) 

--- 
# What have I learned? 2/3

- [re:split](https://github.com/adolfont/pensandoemelixir/blob/main/adventofcode/2021/day14_2021/day14.erl)
![](img/resplit.png) 

![bg 100% right:10%](img/cat2.png) 

---

# How to use AoC to learn Erlang and/or Elixir?

- During December:
  - Set a time limit
  - Try to do both tasks for the day
  - If you can, read others' solutions (on Elixir Forum or Erlang Forums)
  - If not, then either give up or try to complete it  another day

---

# How to use Exercism to learn Erlang and/or Elixir?

- Join the two tracks
- Ask for mentorship whenever possible
  - You can do that with the link
- Look at other people's solutions
- Share your solution on the platform


---

# Final Tips

- "Sharpen the Saw" (Habit 7 - Sharpen the Saw)
- Exercise to stay in shape

---

# Announcements


---

# Code BEAM Brasil 2022

Probably in September. Online.

[https://www.codebeambr.com/](https://www.codebeambr.com/)

![height:12cm](img/cbbr.png)

---

# Elixir Brasil 2022

Probably in September. ???


[https://elixirbrasil.com/](https://elixirbrasil.com/)

![height:12cm](img/exbr.png)


---

# 10 Years of Elixir

You can watch the recording!

Laura Castro gave a testimonial!

[https://bit.ly/10YearsOfElixir](https://bit.ly/10YearsOfElixir)

![height:12cm](img/10yyt.jpeg)

--- 

## Adolfo Neto  [@adolfont on Twitter](http://twitter.com/adolfont)

[adolfont.github.io](http://adolfont.github.io)

![w:200 h:200](img/qr.png)

[Slides](https://github.com/adolfont/pensandoemelixir/blob/main/palestras/2022/codebeam_europe.md)


<!-- https://twitter.com/TheErlef/status/1526616037684281344 -->
