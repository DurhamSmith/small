import boto3
# s3 = boto3.resource('s3')
# # for bucket in s3.buckets.all():
# #     print(bucket.name)

# obj = s3.Object("zs3-demo", "1/info")
# print(obj.get()['Body'].read().decode('utf-8'))

s3 = boto3.client("s3")
all_objects = s3.list_objects(Bucket = 'zs3-demo', Delimiter='/')

s3 = boto3.resource('s3')
pres = all_objects['CommonPrefixes']
d=[]
for pre in pres:
    filename = f'{pre["Prefix"]}info'
    obj = s3.Object("zs3-demo", filename)
    try:
        des = obj.get()['Body'].read()
        print(des)
        print(des.decode('utf-8'))
        d.append((obj.get()['Body'].read().decode('utf-8')))
    except:
        continue
#    print(filename)



    
print(d)    
