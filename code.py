import os
import re
import numpy as np


# Transition matrix with no spaces
def parse_no_space(d):
    transition_matrix = [[0] * 26 for x in range(26)]

    # Go into a file and take every word
    with open(d, 'r') as f:
        lines = f.readlines()
        dict_temp = {}
        for line in lines:
            # Remove punctuation and symbols
            line = re.sub("[^a-zA-Z]+", " ",  line.rstrip()).split()
            for word in line:
                if len(word) > 1:
                    for i, c in enumerate(word.lower()):
                        if i >= 1:
                            index = ord(c) - 97
                            past_index = ord(past) - 97
                            transition_matrix[past_index][index] += 1
                        past = c
    f.close()
    for i in range(0, len(transition_matrix)):
        row = transition_matrix[i]
        row_sum = sum(row)
        if row_sum > 0:
            transition_matrix[i] = [x / row_sum for x in transition_matrix[i]]
    return transition_matrix


# Transition matrix with spaces
def parse_spaces(d):
    transition_matrix = [[0] * 27 for x in range(27)]

    # Go into a file and take every word
    with open(d, 'r') as f:
        lines = f.readlines()
        dict_temp = {}
        for line in lines:
            # Remove punctuation and symbols
            line = re.sub("[^a-zA-Z]+", " ",  line.rstrip()).split()
            for word in line:
                if len(word) > 1:
                    for i, c in enumerate(word.lower()):
                        index = ord(c) - 97
                        if i >= 1:
                            past_index = ord(past) - 97
                            transition_matrix[past_index][index] += 1
                            if i == len(word) - 1:
                                transition_matrix[index][26] += 1
                        else:
                            transition_matrix[26][index] += 1
                        past = c
    f.close()
    for i in range(0, len(transition_matrix)):
        row = transition_matrix[i]
        row_sum = sum(row)
        if row_sum > 0:
            transition_matrix[i] = [x / row_sum for x in transition_matrix[i]]
    return transition_matrix


def compute_score_spaces(string, transition_matrix):
    # Go into a file and take every word
    score = 1
    for i, c in enumerate(string.lower()):
        index = ord(c) - 97
        if i >= 1:
            past_index = ord(past) - 97
            score *= transition_matrix[past_index][index]
            if i == len(string) - 1:
                score *= transition_matrix[index][26]
        else:
            score *= transition_matrix[26][index]
        past = c
    return score


def compute_convergence(encrypted, decrypted, transition_matrix):
    count = 0
    while not encrypted.equals(decrypted):
        score_encrypted = compute_score_spaces(encrypted)

        encrypted = encrypted.replace('a', '!')
        encrypted = encrypted.replace('b', 'a')
        new = encrypted.replace('!', 'b')
        score_new = compute_score_spaces(new)
        acceptance = score_new/score_encrypted
        if np.random.uniform(0,1,1)[0] < acceptance:
            encrypted = new
        count += 1
    return count


# Perform experiment 1
def experiment_one():
    english = os.getcwd() + '/raw_data.txt'

    with open('Results.txt', 'w') as output:
        print('English Transition Matrix', file=output)
        transition_matrix = parse_no_space(english)
        transition_matrix = parse_spaces(english)
    output.close()

experiment_one()