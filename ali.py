#!/usr/bin/env python
# coding=utf-8
‘’‘
阿里云一键创建服务器脚步
’‘’
import json
import time
import traceback
import datetime
import sys

from dateutil.parser import parse
from aliyunsdkcore.client import AcsClient
from aliyunsdkcore.acs_exception.exceptions import ClientException, ServerException
from aliyunsdkecs.request.v20140526.RunInstancesRequest import RunInstancesRequest
from aliyunsdkecs.request.v20140526.DescribeInstancesRequest import DescribeInstancesRequest


RUNNING_STATUS = 'Running'
CHECK_INTERVAL = 1
CHECK_TIMEOUT = 180

class AliyunRunInstancesExample(object):

    def __init__(self):
        self.access_id = ''
        self.access_secret = ''

        # 是否只预检此次请求。true：发送检查请求，不会创建实例，也不会产生费用；false：发送正常请求，通过检查后直接创建实例，并直接产生费用
        self.dry_run = False
        # 实例所属的地域ID
        self.region_id = 'cn-hongkong'
        # 实例的资源规格
        self.instance_type = 'ecs.t5-lc2m1.nano'
        # 实例的计费方式
        self.instance_charge_type = 'PostPaid'
        # 镜像ID
        self.image_id = 'm-j6cd1j3gplqpeid8rvwv'
        # 指定新创建实例所属于的安全组ID
        self.security_group_id = 'sg-j6cggladpa2096g62cab'
        # 购买资源的时长
        self.period = 1
        # 购买资源的时长单位
        self.period_unit = 'Hourly'
        # 实例所属的可用区编号
        self.zone_id = 'random'
        # 网络计费类型
        self.internet_charge_type = 'PayByTraffic'
        # 虚拟交换机ID
        self.vswitch_id = 'vsw-j6cqejqhrb9t63nh4yhl7'
        # 实例名称
        self.instance_name = hostname
        # 是否使用镜像预设的密码
        self.password_inherit = True
        # 指定创建ECS实例的数量
        self.amount = 1
        # 公网出带宽最大值
        self.internet_max_bandwidth_out = 100
        # 云服务器的主机名
        self.host_name = hostname
        # 是否为实例名称和主机名添加有序后缀
        self.unique_suffix = True
        # 是否为I/O优化实例
        self.io_optimized = 'optimized'
        # 后付费实例的抢占策略
        self.spot_strategy = 'SpotWithPriceLimit'
        # 设置实例的每小时最高价格
        self.spot_price_limit = 0.05
        # 自动释放时间
        self.auto_release_time = (datetime.datetime.now()+datetime.timedelta(hours=4)).strftime("%Y-%m-%dT%H:%M:%SZ")
        # 系统盘大小
        self.system_disk_size = '20'
        # 系统盘的磁盘种类
        self.system_disk_category = 'cloud_efficiency'

        self.client = AcsClient(self.access_id, self.access_secret, self.region_id)

    def run(self):
        try:
            ids = self.run_instances()
            self._check_instances_status(ids)
        except ClientException as e:
            print('Fail. Something with your connection with Aliyun go incorrect.'
                  ' Code: {code}, Message: {msg}'
                  .format(code=e.error_code, msg=e.message))
        except ServerException as e:
            print('Fail. Business error.'
                  ' Code: {code}, Message: {msg}'
                  .format(code=e.error_code, msg=e.message))
        except Exception:
            print('Unhandled error')
            print(traceback.format_exc())

    def run_instances(self):
        """
        调用创建实例的API，得到实例ID后继续查询实例状态
        :return:instance_ids 需要检查的实例ID
        """
        request = RunInstancesRequest()

        request.set_DryRun(self.dry_run)

        request.set_InstanceType(self.instance_type)
        request.set_InstanceChargeType(self.instance_charge_type)
        request.set_ImageId(self.image_id)
        request.set_SecurityGroupId(self.security_group_id)
        request.set_Period(self.period)
        request.set_PeriodUnit(self.period_unit)
        request.set_ZoneId(self.zone_id)
        request.set_InternetChargeType(self.internet_charge_type)
        request.set_VSwitchId(self.vswitch_id)
        request.set_InstanceName(self.instance_name)
        request.set_PasswordInherit(self.password_inherit)
        request.set_Amount(self.amount)
        request.set_InternetMaxBandwidthOut(self.internet_max_bandwidth_out)
        request.set_HostName(self.host_name)
        request.set_UniqueSuffix(self.unique_suffix)
        request.set_IoOptimized(self.io_optimized)
        request.set_SpotStrategy(self.spot_strategy)
        request.set_SpotPriceLimit(self.spot_price_limit)
        request.set_AutoReleaseTime(self.auto_release_time)
        request.set_SystemDiskSize(self.system_disk_size)
        request.set_SystemDiskCategory(self.system_disk_category)

        body = self.client.do_action_with_exception(request)
        data = json.loads(body)
        #print data
        instance_ids = data['InstanceIdSets']['InstanceIdSet']
        print('Success. Instance creation succeed. InstanceIds: {}'.format(', '.join(instance_ids)))
        self.instance_idss = list(instance_ids)
        #print self.instance_idss
        return instance_ids

    def _check_instances_status(self, instance_ids):
        """
        每3秒中检查一次实例的状态，超时时间设为3分钟。
        :param instance_ids 需要检查的实例ID
        :return:
        """
        start = time.time()
        while True:
            request = DescribeInstancesRequest()
            request.set_InstanceIds(json.dumps(instance_ids))
            body = self.client.do_action_with_exception(request)
            data = json.loads(body)
            for instance in data['Instances']['Instance']:
                if RUNNING_STATUS in instance['Status']:
                    instance_ids.remove(instance['InstanceId'])
                    print('Instance boot successfully: {}'.format(instance['InstanceId']))

            if not instance_ids:
                print('Instances all boot successfully')
                break

            if time.time() - start > CHECK_TIMEOUT:
                print('Instances boot failed within {timeout}s: {ids}'
                      .format(timeout=CHECK_TIMEOUT, ids=', '.join(instance_ids)))
                break

            time.sleep(CHECK_INTERVAL)
        #print self.instance_idss
        self.instances_ip(self.instance_idss)

    def instances_ip(self, instance_ids):
        request = DescribeInstancesRequest()
        request.set_InstanceIds(json.dumps(instance_ids))
        body = self.client.do_action_with_exception(request)
        data = json.loads(body)
        #print data
        for i in range(self.amount):
            #print 'IP:'+data['Instances']['Instance'][i]['PublicIpAddress']['IpAddress'][0]
            print 'ssh root@'+data['Instances']['Instance'][i]['PublicIpAddress']['IpAddress'][0]
            newline='alias '+hostname+'="ssh root@'+data['Instances']['Instance'][i]['PublicIpAddress']['IpAddress'][0]+'"\n'
            with open('alias','a') as file:
                file.write(newline)
        time.sleep(1)

    def instances_list(self):
        request = DescribeInstancesRequest()
        #request.set_InstanceIds(json.dumps(instance_ids))
        body = self.client.do_action_with_exception(request)
        data = json.loads(body)
        print '-------目前服务器列表:'
        for item in data['Instances']['Instance']:
            #print item['AutoReleaseTime']
            if item['AutoReleaseTime']!='':
                print item['InstanceName']+'\t'+str(item['PublicIpAddress']['IpAddress'][0])+'\t'+(parse(item['AutoReleaseTime'])+datetime.timedelta(hours=8)).strftime("%Y-%m-%d %H:%M")
            else:
                print item['InstanceName']+'\t'+str(item['PublicIpAddress']['IpAddress'][0])

if __name__ == '__main__':
    if len(sys.argv)<2:
    	#hostname = 'guanying'
        print 'parameter hostname not set'
        sys.exit()
    else:
    	hostname = sys.argv[1]
        print hostname

    AliyunRunInstancesExample().run()
    AliyunRunInstancesExample().instances_list()
    #AliyunRunInstancesExample().instances_ip([u'i-j6cijisu0fkpnrfu3vci'])
