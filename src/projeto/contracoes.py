def clean_and_split_ents(entlist):
    for ent in entlist:
        yield ent.replace('\n', '').strip().split(' ')

def remove_uppercase_tks(entlist):
    lowers = []
    for ent in entlist:
        for token in ent:
            if token.islower():
                lowers.append(token)
    return lowers
