
import requests
from lxml import etree
from time import time, sleep, localtime, strftime
import jsonpath
import json
import os
import pandas as pd

os.environ['NO_PROXY'] = 'www.xiaohongshu.com'

headers = {
    'authority': 'www.xiaohongshu.com',
    'accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7',
    'accept-language': 'zh-CN,zh;q=0.9',
    'cache-control': 'max-age=0',
    'cookie': 'a1=18bfa5a86f6noy48pm8b592xug3bxkeyjv3b02qxp30000526147; webId=d8e4b561e8e29f60d631c139192a4ddd; gid=yYDi020Y08VfyYDi020YKhJAiKIlv4YU6YD2qCUjJC7kqDq8DCTdv88882JKy4W8YJJq04qW; abRequestId=d8e4b561e8e29f60d631c139192a4ddd; web_session=0400694782b1b66fe991b90e5d374beb25902d; unread={%22ub%22:%226568b531000000003c01018e%22%2C%22ue%22:%22654e16f40000000017034af3%22%2C%22uc%22:28}; webBuild=3.18.2; xsecappid=xhs-pc-web; websectiga=f47eda31ec99545da40c2f731f0630efd2b0959e1dd10d5fedac3dce0bd1e04d; sec_poison_id=2e446249-d80e-456b-af79-67927189fee1',
    'sec-ch-ua': '"Chromium";v="118", "Google Chrome";v="118", "Not=A?Brand";v="99"',
    'sec-ch-ua-mobile': '?0',
    'sec-ch-ua-platform': '"Windows"',
    'sec-fetch-dest': 'document',
    'sec-fetch-mode': 'navigate',
    'sec-fetch-site': 'same-origin',
    'sec-fetch-user': '?1',
    'upgrade-insecure-requests': '1',
    'user-agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/118.0.0.0 Safari/537.36',
}


def parse_timestamp(timestamp):
    if not timestamp:
        return None
    # 转换成localtime
    time_local = localtime(timestamp / 1000)
    # 转换成新的时间格式(精确到秒)
    dt = strftime("%Y-%m-%d %H:%M:%S", time_local)
    return dt


def get_detail_by_note_id(note_id):
    start_time = time()
    note_url = f'https://www.xiaohongshu.com/explore/{note_id}'
    response = requests.get(url=note_url, headers=headers)
    html = etree.HTML(response.text)
    target_text = html.xpath('//script[contains(text(), "INITIAL_STATE")]/text()')

    if target_text:
        result = target_text[0]

        if result[0] == '{' and result[-1] == '}':
            text = result
        else:
            # 原来的接口
            text = result[result.index('{'):result.rindex('}') + 1]

        text = text.replace('undefined', 'null')

        json_data = json.loads(text)

        note_detail = jsonpath.jsonpath(json_data, '$..note..note')

        if note_detail:
            note_detail = note_detail[0]

            if note_detail == {}:
                print(f'笔记 https://www.xiaohongshu.com/explore/{note_id} 状态异常')
                print(json_data)
                sleep(6)
                return None
            print(note_detail)

            interact_info = note_detail['interactInfo']

            user = note_detail.get('user', None)

            print(f'cost time: {time() - start_time} sec')

            ip_location = note_detail.get('ipLocation', '')

            video_url_entity = jsonpath.jsonpath(json_data, '$..media..masterUrl')
            if video_url_entity:
                video_url = video_url_entity[0]
            else:
                video_url = ''

            return {
                'app_note_id': [note_detail.get('noteId')],
                'app_publish_time': [parse_timestamp(note_detail.get('time', None))],
                'app_update_time': [parse_timestamp(note_detail.get('lastUpdateTime', None))],
                'app_title': [note_detail.get('title')],
                'app_note_desc': [note_detail.get('desc')],
                'app_content_desc': [note_detail.get],
                'app_ip_location': [ip_location],
                'app_video_url': [video_url],
                'app_at_list': [json.dumps(note_detail.get('atUserList', []), indent=2, ensure_ascii=False)],
                'app_hashtag_list': [json.dumps(note_detail.get('tagList', []), indent=2, ensure_ascii=False)],
                'app_like_cnt': [interact_info.get('likedCount')],
                'app_comment_cnt': [interact_info.get('commentCount')],
                'app_collect_cnt': [interact_info.get('collectedCount')],
                'app_share_cnt': [interact_info.get('shareCount')],
                'app_note_type': [note_detail.get('type')],
                'app_user_id': [user.get('userId')],
                'app_user_name': [user.get('nickname')]
            }

        else:
            print(f'{text}')
            return None

    else:
        print(f'{note_url} 状态异常 empty')
        return None


if __name__ == '__main__':
    input_file = '/Users/Lynn/Downloads/crawl/5c3a0b93a1710a00014c1914.csv'
    df = pd.read_csv(input_file)
    app_cols = [
        'app_note_id',
        'app_publish_time',
        'app_update_time',
        'app_title',
        'app_note_desc',
        'app_ip_location',
        'app_video_url',
        'app_at_list',
        'app_hashtag_list',
        'app_like_cnt',
        'app_comment_cnt',
        'app_collect_cnt',
        'app_share_cnt',
        'app_note_type',
        'app_user_id',
        'app_user_name',
    ]
    has_crawl_detail_col = 'has_crawl_detail'
    if app_cols[4] not in df.columns.values.tolist():
        for app_col in app_cols:
            df[app_col] = ['' for _ in range(df.shape[0])]
        df[has_crawl_detail_col] = ['no' for _ in range(df.shape[0])]

    for index, row in df.iterrows():
        print(f'{index + 1}/{df.shape[0]}')
        note_id = row['note_id']
        has_crawl_detail = row[has_crawl_detail_col]
        if has_crawl_detail == 'yes':
            continue
        app_res = get_detail_by_note_id(note_id=note_id)
        sleep(3)
        if app_res:
            for app_col in app_cols:
                df.loc[index, app_col] = app_res.get(app_col)
            df.loc[index, has_crawl_detail_col] = 'yes'

            df.to_csv(input_file, index=False, encoding='utf-8-sig')
