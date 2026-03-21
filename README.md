<div align="center">
  <a href="./LICENSE">
    <img src="https://img.shields.io/github/license/we-data-ch/typr" alt="License">
  </a>
  <a href="https://github.com/we-data-ch/typr/issues">
    <img src="https://img.shields.io/github/issues/we-data-ch/typr" alt="Issues">
  </a>
  <a href="https://github.com/we-data-ch/typr/stargazers">
    <img src="https://img.shields.io/github/stars/we-data-ch/typr" alt="Stars">
  </a>
  <a href="https://github.com/we-data-ch/typr/commits/main">
    <img src="https://img.shields.io/github/last-commit/we-data-ch/typr" alt="Last Commit">
  </a>
</div>

# TypR
![](src/TypR_logo.png)

## Website

The link for the official website is [here](https://we-data-ch.github.io/typr.github.io/).

## Installation

You can see it through the getting started of the documentation [here](https://we-data-ch.github.io/typr.github.io/docs/intro/).

## Code example

```scala
type Person = list {
	name: char,
	age: int
};

new_person <- fn(name: char, age: int): Person {
	list(name = name, age = age)
};

is_minor <- fn(p: Person): bool {
	p$age < 18
};

alice <- new_person("Alice", 35);

alice.is_minor()
```


## Support this project 🚀

Hey awesome scientist!  

If this project sparks your curiosity, makes your life easier, or simply inspires you, here’s how you can help it grow:

- 🛠️ **Contribute**: improve the code, report bugs , or suggest cool new features
- 📢 **Share** it with friends, colleagues, or your community — spread the word!

Every little action counts — together we can make this project shine even brighter. 
Thank you✨  
