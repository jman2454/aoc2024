if __name__ == '__main__':
    import os 
    import re

    parent = os.path.split(__file__)[0]
    dirs = os.listdir(parent)
    dirs = [d for d in dirs if os.path.isdir(d)]
    dirs = [ re.match(r'day(\d+)(_2)?', d) for d in dirs ]
    dirs = [ (int(d.groups()[0]), (1 if d.groups()[1] else 0)) for d in dirs if d ]
    last = max(dirs, key=lambda x: x[0] * 2 + x[1])
    
    prev_puzzle_name = f'day{last[0]}' + ('_2' if last[1] else '')

    print(f'Last puzzle was {prev_puzzle_name}')

    next_puzzle_name = ''
    if last[1]:
        next_puzzle_name = f'day{last[0] + 1}'
    else:
        next_puzzle_name = f'day{last[0]}_2'

    next_dir = os.path.join(parent, next_puzzle_name)

    if not os.path.exists(next_dir):
        os.mkdir(next_dir)

    dune_file = os.path.join(next_dir, 'dune')
    with open(dune_file, "w") as config:
        config.write(f"""(executable
 (public_name {next_puzzle_name})
 (name main)
 (libraries aoc))
""")
        
    with open(os.path.join(next_dir, 'main.ml'), "w") as main:
        main.write('let () = ()')