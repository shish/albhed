-- test
import List
import Debug.Trace

indexof :: Eq a => [a] -> a -> Int
indexof array element = indexof1 array element 0
	where
		indexof1 (x:xs) element num = 
			if x == element then num
			else indexof1 xs element (num + 1)

-- Turn a string of letters into a generic map of numbers
-- cat=123, add=122, bob=121
vmap :: String -> [Int]
vmap word = map (indexof word) word

-- Check if two vmaps are the same
same_vmap :: (String, [Int]) -> (String, [Int]) -> Bool
same_vmap vmap1 vmap2 = (snd vmap1) == (snd vmap2)

get_translations :: [(String, [Int])] -> (String, [Int]) -> [(String, [Int])]
-- get_translations english_vmap_list input_vmap = filter (same_vmap input_vmap) english_vmap_list
get_translations english_vmap_list input_vmap = 
--	Debug.Trace.trace (
--		show (fst input_vmap) ++
--		" matches " ++
--		show (length (map fst (filter (same_vmap input_vmap) english_vmap_list))) ++
--		" words")
	(filter (same_vmap input_vmap) english_vmap_list)

create_rules :: [(String, [String])] -> [[(Char, Char)]]
create_rules input_and_outputs = map create_rules_0 input_and_outputs
	where
		create_rules_0 input_and_outputs = unique (create_rules_1 input_and_outputs)

		create_rules_1 :: (String, [String]) -> [(Char, Char)]
		create_rules_1 (input, []) = []
		create_rules_1 (input, (output:outputs)) = (create_rules_from_io input output) ++ create_rules_1 (input, outputs)

		create_rules_from_io :: String -> String -> [(Char, Char)]
		create_rules_from_io input output = zip input output 

-- 'a' [('a', 'b')] -> True, rule_set has a rule for 'a'
-- 'b' [('a', 'b')] -> False, rule_set does not have a rule for 'a'
rule_set_has_rule_for :: Eq a => a -> [(a, a)] -> Bool
rule_set_has_rule_for char rule_set = elem char (map fst rule_set)

-- ('a', 'b') [('a', 'b')] -> True, rule_set agrees with rule
-- ('a', 'b') [('a', 'c')] -> False, rule_set disagrees with rule
-- ('a', 'b') [('b', 'c')] -> True, rule_set has no opinion on what to do with 'a', so it passes
rule_set_agrees_with :: Eq a => (a, a) -> [(a, a)] -> Bool
rule_set_agrees_with rule rule_set = (elem rule rule_set) || not (rule_set_has_rule_for (fst rule) rule_set)

-- apply "rule_set_agrees_with rule" to all rule sets
all_rule_sets_agree_with :: Eq a => [[(a, a)]] -> (a, a) -> Bool
all_rule_sets_agree_with all_rule_sets rule = and (map (rule_set_agrees_with rule) all_rule_sets)

get_good_rules :: Eq a => [[(a, a)]] -> [(a, a)] -> [(a, a)]
get_good_rules all_rule_sets rule_set = filter (all_rule_sets_agree_with all_rule_sets) rule_set

unique :: Eq a => [a] -> [a]
unique [] = []
unique (x:xs) =
	if elem x xs
		then unique xs
		else x : unique xs

valid_translation :: Eq a => [(a, a)] -> [a] -> [a] -> Bool
valid_translation rules [] [] = True
valid_translation rules (x:xs) (y:ys) = 
	if elem (x, y) rules
		then valid_translation rules xs ys
		else False

-- get_valid_word :: [(a, a)] -> [(String, [Int])] -> (String, [Int]) -> String
get_valid_word rules english_vmaps input_vmap =
		filter
			(valid_translation rules (fst input_vmap)) -- filter for things which match rules
			(map fst (get_translations english_vmaps input_vmap)) -- all possible translations

main = do
	input <- readFile "input.txt"
	let input_words = words input
	let input_vmaps = zip input_words (map vmap input_words)

	dict <- readFile "/usr/share/dict/words"
	let english_words = words dict
	let english_vmaps = zip english_words (map vmap english_words)

	-- t_p = for each word of input, we now have an array of possible translations + vmaps
	let translated_possibilities = map (get_translations english_vmaps) input_vmaps

	-- just get an array of possible translations
	let translation_results = map (map fst) translated_possibilities

	-- an array of (word, [translations])
	let input_and_outputs = zip input_words translation_results

	-- each input word gives us a set of (a, a') rules, so this is [[(a, b), (b, c)], [(a, d), (g, h)]]
	-- vowel_rules is an optimisation coming from the observation that vowels always map to vowels
	-- known_rules is cheating :P
	let vowel_rules = [[
		('a', 'e'), ('a', 'i'), ('a', 'o'), ('a', 'u'), ('a', 'y'),
		('e', 'a'), ('e', 'i'), ('e', 'o'), ('e', 'u'), ('e', 'y'),
		('i', 'a'), ('i', 'e'), ('i', 'o'), ('i', 'u'), ('i', 'y'),
		('o', 'a'), ('o', 'e'), ('o', 'i'), ('o', 'u'), ('o', 'y'),
		('u', 'a'), ('u', 'e'), ('u', 'i'), ('u', 'o'), ('u', 'y'),
		('y', 'a'), ('y', 'e'), ('y', 'i'), ('y', 'o'), ('y', 'u')
		]]
	let known_rules = [[
		('c', 's'), ('u', 'o'), ('n', 'r'), ('o', 'y')
		]]
	let possible_rules = create_rules input_and_outputs ++ vowel_rules ++ known_rules

	-- for each rule, check that the other words agree with the rule
	let good_rules = unique (concat (map (get_good_rules possible_rules) possible_rules))

	print "Possible rules:"
	print (List.sort (good_rules))

	let output_words = map (get_valid_word good_rules english_vmaps) input_vmaps

	print output_words
--	print (foldr (++) "" (map (++ " ") (show output_words)))
