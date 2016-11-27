---
layout: post
author: Nicole Rauch
# meta: Meta Data Goes Here
title: Storing Passwords Safely
lang: en
---

In order to store passwords on a computer, it is possible but not advisable to save them in cleartext. The problem with cleartext is that once the machine gets compromised, the attacker has access to the passwords of all users at once, and they can use them to gain more powers on the compromised system or to try to access other accounts if they can be linked to that user, in the hope that the same password was used there as well.


So we need to somehow conceal the passwords. This has literally been a problem for ages [[1][1]], and one of the first approaches, the Caesar cipher [[2][2]], was already invented by the Romans about 2000 years ago (ROT13 is a variant of it). Of course, this is much too simplistic for our purposes.


A better approach would be to encrypt the passwords, either with a synchronous or an asynchronous encryption method. If the encryption method is strong enough, the passwords cannot easily be recovered from the encrypted result. Still, there is a downside because encryption is meant to be reversible via the encryption key, and if the key gets compromised, all passwords are accessible at once.


That is why nowadays all serious systems use a different mechanism called password hashing [[3][3], [4][4]]. Password hashing is a method that works only in one direction - a password can be turned into a hash digest, but a hash digest can never be turned back into a password. On login, the password the user provides is hashed and compared to the stored hash digest. The only way to attack hashed passwords is by either comparing them to an already compromised hash digest, or via brute-force attacks where a large number of inputs (either words taken from a dictionary or just any combination of characters up to a certain length) is hashed and their hash digests are compared to the ones that are to be cracked.


To make these brute-force attacks more expensive, hashes are usually salted [[5][5]]. A salt is a random string of fixed length that is different for each password. The simplest approach is to concatenate the salt to the password before encrypting it, and to store the salt together with the hash digest (usually by prefixing it to the digest). This way, the same password usually ends up having different hash digests for different users and across servers because the salts usually differ. This way, it is infeasible to run a dictionary attack against all users' passwords at once or to simply compare the hash digests with digests that were already pre-calculated for the dictionary's words - the attack needs to generate the hash digests for each salt individually, thus making the attack much more costly.


This article presents an overview of some widely known hashing algorithms, how they work and how much security they can provide.

## MD5
### Algorithm

MD5 [[7][7]] was first published in 1992 and is based on the Merkle-Damgård construction [[6][6]] which also gave it its name. This algorithm first breaks the input into blocks of fixed size (padding the last block if necessary) and then folds a compression function over the sequence of blocks.

For MD5, an input block length of 512 bits is used. The message is padded with a so-called length-encoding padding. The leftmost bit of the sequence that is attached to the message contains a 1, the rightmost 64 bits contain the length of the original message modulo 2^64, and the remaining bits between these two (if any) are filled with zeroes. If there are less than 65 bits that need to be padded, the padding is extended to the next block.

The MD5 fold operation is initialized with an 128-bit value. The folding function breaks up the input block into four parts of 128 bits each and then combines these with the accumulator through a series of bitwise logical operations. The resulting 128-bit value is the MD5 hash digest.

### Security considerations

Collision resistance is one of the most important properties of a good, i.e. secure hashing algorithm. It states that it should be difficult to find two input messages with the same hash digest.

Meanwhile it has been discovered that MD5 is not collision-resistant and that it contains some other serious security flaws. Therefore, it is recommended to not use it any more for security purposes.

## SHA-1
### Algorithm

The SHA-1 algorithm [[10][10]] was first published in 1995. It is very similar to the MD5 algorithm, but some important modifications made it stronger than its predecessor:

   * The hash digest is 160 bits long instead of 128.
   * The calculation includes an additional bit rotation step which is applied in order to lower the risk of collisions.

and some other minor changes. These changes make it more collision resistant than MD5.

### Security Considerations

Although the algorithm has proven to be more secure than MD5, a successful attack which can find collisions was presented in 2005. Therefore, this algorithm is no longer advisable for password hashing either.

## BCrypt
### Algorithm

BCrypt [[8][8], [9][9]] is a hashing algorithm that was designed for hashing passwords. One of its most striking properties is that it is rather slow (about 10000 times slower than SHA-1) and that it is configurable in speed - this way, it can be adjusted such that the speed reduction is unnoticeable in regular usage but makes a brute-force attack very long-winding. It also allows for the adaption to faster hardware, so that the time the hashing algorithm takes will remain the same although the hardware has become faster.

Additionally, a salt is built into the algorithm to prevent attacks with pre-calculated dictionaries (which are much more attractive for such a slow algorithm).

The algorithm consists of two phases: Generation and application of an encryption key. This is a variation of the Blowfish algorithm (invented by Bruce Schneier). The authors dubbed it Eksblowfish which stands for "expensive key schedule blowfish" because the key generation part is cost parameterizable.

The most interesting aspect of this algorithm is that it does not actually encrypt the user's password; it uses the password and a salt to generate the encryption key and then uses this key to encrypt the constant message "OrpheanBeholderScryDoubt". This means that the "contents" of the hash digest is already known, but it is the way in which it gets manipulated that depends on the input message.

To make the key generation part cost parameterizable, the algorithm applies 2^n modification rounds where n is the cost parameter. In each round, the key generated so far is modified using the user’s input and the salt. This means that the resulting key depends on all three factors; it can only be recreated by applying the same number of iterations, which allows to adjust the time it takes to hash a password.

### Security Considerations

Although Bruce Schneier, the inventor of Blowfish, said in 2007 that he is surprised to see Blowfish still being widely used, and that he recommends using Twofish instead, no successful attack against Bcrypt is known to date. This explains its ongoing popularity as it is the default password hash algorithm for OpenBSD and other systems, including some Linux distributions.


[1]: https://en.wikipedia.org/wiki/History_of_cryptography
[2]: https://en.wikipedia.org/wiki/Caesar_cipher
[3]: http://security.blogoverflow.com/2011/11/why-passwords-should-be-hashed/
[4]: https://en.wikipedia.org/wiki/Cryptographic_hash_function
[5]: https://en.wikipedia.org/wiki/Salt_(cryptography)
[6]: https://en.wikipedia.org/wiki/Merkle%E2%80%93Damg%C3%A5rd_construction
[7]: https://en.wikipedia.org/wiki/MD5
[8]: http://dustwell.com/how-to-handle-passwords-bcrypt.html
[9]: http://www.openbsd.org/papers/bcrypt-paper.ps
[10]: https://en.wikipedia.org/wiki/SHA-1
