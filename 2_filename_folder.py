import glob, os, shutil

folder = os.getcwd()

for file_path in glob.glob(os.path.join(folder, '*.epw')):
    new_dir = file_path.rsplit('.', 1)[0]
    os.mkdir(os.path.join(folder, new_dir))
    shutil.move(file_path, os.path.join(new_dir, os.path.basename(file_path)))