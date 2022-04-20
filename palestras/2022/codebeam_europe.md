
# Learning Elixir and Erlang with Advent of Code and Exercism: advantages and challenges

*Date: 04/06/2022. Time: 8:00 (GMT-3). Link: https://youtu.be/ENifBtn3UUw*

Adolfo Neto

@adolfont (Twitter, Instagram, Telegram, GitHub)

https://adolfont.github.io/

https://youtube.com/c/AdolfoNeto


---

# Presentation script

- Who am I?
- What is Elixir? *(quickly)*
- What is Erlang? *(quickly)*
- What is Advent of Code?
- What is Exercise?
- What is good/bad in AoC/Exercism?
- **How ​​to use AoC and/or Exercism to learn Erlang and/or Elixir?**

---


# Who am I?

- Professor at UTFPR
    - Free EC/BSI degree
    - Free PPGCA Professional Master's
- Co-host of Elixir in Focus
- Co-host of Fronteiras da Engenharia de Software
- Member of the Erlang Ecosystem Foundation Education WG
- Two YouTube channels
  - https://youtube.com/c/AdolfoNeto
  - https://www.youtube.com/c/ElixirErlangandtheBEAMwithAdolfoNeto
- Hello, Erlang!, Emílias Podcast: Mulheres na Computação, Professor Adolfo Neto podcast
- ...
---

# Elixir

- Programming language created by a Brazilian, José Valim, in 2011/2012
- Runs on Erlang's virtual machine (VM), BEAM
- Concurrent functional paradigm
- Not purely functional
- No big company behind (until 2020 there was an average, Plataformatec)


---

# Erlang

- Programming language created by three people (Joe, Mike and Robert) at Ericsson (Sweden) in 1986
- Open Source since 1998
- To this day Erlang's virtual machine (VM), BEAM, is maintained by people from Ericsson and other companies
- Concurrent functional paradigm
- Not purely functional

---

# What are the difficulties in learning Elixir and Erlang?

- Some are common to almost all programming languages

## What helps? (in my opinion)

- Read books or well-structured materials (Elixir School)
- Have a mentor
- Chat with other people
- Participate in the community

---

# What are the difficulties in learning Elixir and Erlang?
## What helps? (in my opinion)

- Putting knowledge into practice - just above your current level

- Write code idiomatically, the way more experienced people in the community write

---

# Which is most used? I don't know, but...

"Have you ever solved an Advent of Code or an Exercise challenge?"

"Have you ever solved a programming challenge from:"


- [Twitter Poll](https://twitter.com/elixiremfoco/status/1510973616602722308)
- [Poll on Elixir Forum](https://elixirforum.com/t/have-you-ever-solved-an-advent-of-code-or-an-exercism-challenge/46985)
- [Poll on Erlang Forums](https://erlangforums.com/t/have-you-ever-solved-an-advent-of-code-or-an-exercism-challenge/1285)
- [Added](https://docs.google.com/spreadsheets/d/1COt5PrCVjm7hnNTBWC27UI9FsVr_qYOkkocWNZE1T5I/edit?usp=sharing)

---

# What is the Advent of Code?

- Website: https://adventofcode.com/
- Wikipedia: https://en.wikipedia.org/wiki/Advent_of_Code
- Creator: [Eric Wastl](http://was.tl/)

- Annual “Event” from December 1st to December 25th
  - It's a kind of competition
  - There are private leaderboards
  - What counts for the ranking is who finds the answer, usually a number, first
- So the time zone counts.

---

# Me in Advent of Code

https://adventofcode.com/

- 2021: 1 to 10, 13 and 14 - total 12x2 = 24
- 2020: 1 to 9 and part 1 of day 10: total 19
- 2019: 1 and 2. Total: 4


Where am I saving my solutions as of 2021
https://github.com/adolfont/pensandoemelixir/tree/main/adventofcode/

---

# Important!

- long statement
- Pay close attention to the wording!
- Undisclosed difficulty
- Many adhoc solutions
    - Example: [Day 2 Part 1 2019](https://adventofcode.com/2019/day/2)


---

# What is Exercise?

- Created by [Katrina Owen](https://www.kytrinyx.com/)
    - read https://www.kytrinyx.com/exercism/!!!
    - and https://en.wikipedia.org/wiki/Exercism

- Exercises + free mentoring
- Gives you description and tests in one language
- It's in version 3

Access https://exercism.org/

---

# Mentoring example

Private link:
https://exercism.org/mentoring/discussions/ad416ef10f1e42b4b97445f2ee672f53

---

# José Valim and the Advent of Code

- https://www.twitch.tv/josevalim
- https://www.youtube.com/playlist?list=PLNP8vc86_-SOV1ZEvX_q9BLYWL586zWnF

---

# Good/bad AoC

- What's good about AoC?
    - community
- What is "bad" in AoC?
    - weekend
    - December!
    - some require a lot of prior knowledge (marathons and/or programming competitions)
    - stimulus to unreadable code

---

# Good/bad Exercise

- What is good about Exercism?
    - mentoring
    - tests
    - automatic feedback
- What is "bad" in Exercism?
    - tests already ready
    - v3 kind of discourages mentoring

---

# How to use AoC to learn Erlang and/or Elixir?

- During the month of December:
  - Set time limit
  - Try to do both tasks for the day
  - If you can, read others' solutions (on Elixir Forum or Erlang Forums)
  - If not, then
  - ida gives up or tries to complete another day

---

# How to use Exercism to learn Erlang and/or Elixir?

- Enter the two tracks
- Ask for mentorship whenever possible
  - You can order with the link
- Look at other people's solutions
- Share your solution on the platform

---

# What have I learned?

- Use of Enum (Valim) #aoc #elixir
- Functions with the same name but different arity, one being public and the other private #erlang #exercism
- various uses of [*list comprehension*](https://www.erlang.org/doc/programming_examples/list_comprehensions.html):
`[command(S) || S <- string:split(String, [$\n], all), S =/= ""].`
- [case](https://github.com/adolfont/pensandoemelixir/blob/main/adventofcode/2021/day02_2021/day02_2021_v2.erl) #erlang #aoc
- [read, understand and run code from others](https://github.com/adolfont/pensandoemelixir/blob/main/adventofcode/2021/day04_2021/danilagamma.erl) #erlang #aoc
- [re:split](https://github.com/adolfont/pensandoemelixir/blob/main/adventofcode/2021/day14_2021/day14.erl)

https://github.com/adolfont/pensandoemelixir/tree/main/adventofcode

---

# Final Tips

- "Sharpen the Saw" (Habit 7 - Sharpen the Saw)
- Exercise to stay in shape
   - → Calistenicoders https://t.me/calistenicoders
