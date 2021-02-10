import os

cwd = os.getcwd()
file_list = os.listdir(cwd)
#print(file_list)

for file_name in file_list:
    if file_name.endswith('.epw'): os.rename(file_name, file_name[3:-17]+'.epw')