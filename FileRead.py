def readfile():
    with open("detroit-blight-violations-mini.csv") as f:
        i = 0
        for line in f:
            i += 1
            if i == 1:
                continue
            dataRow = line.split(",")
            lat = dataRow[-2]
            lon = dataRow[-3]
            print(lon,lat)

import pandas as pd
def pdtest():
    df = pd.read_csv("detroit-blight-violations-mini.csv")
    print(df[1])
    
            
def main():
##    readfile()
    pdtest()

if __name__ == '__main__':
    main()
