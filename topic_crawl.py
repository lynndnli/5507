"""
Created on Sat Nov 11 12:43:22 2023

@author: Lynn
"""

import requests

import os

from time import sleep, localtime, strftime

import pandas as pd

import json

import traceback

from config import headers

slp_tms_per_request = 6

data_folder = './topic'

os.environ['NO_PROXY'] = 'xiaohongshu.com'

if not os.path.exists(data_folder):
    os.mkdir(data_folder)


def parse_timestamp(timestamp):
    if not timestamp:
        return None
    # 转换成localtime
    time_local = localtime(timestamp / 1000)
    # 转换成新的时间格式(精确到秒)
    dt = strftime("%Y-%m-%d %H:%M:%S", time_local)
    return dt


def get_image_list(image_ele):
    return [image.get('url_size_large') for image in image_ele]


def save_data(resp_json, topic_id):
    result_file = os.path.join(data_folder, f'{topic_id}.csv')

    if os.path.exists(result_file):
        df = pd.read_csv(result_file)
    else:
        df = pd.DataFrame({
            'note_id': [],
            'note_link': [],
            'title': [],
            'publish_time': [],
            'author_id': [],
            'author_name': [],
            'author_link': [],
            'image_list': [],
            'image_count': [],
            'collected_count': [],
            'liked_count': [],
            'note_type': [],
        })

    items = resp_json.get('data', {}).get('notes', {})

    for item in items:
        print(item)
        note_id = item.get('id')
        title = item.get('title', '')
        liked_count = item.get('likes', '')

        user = item['user']
        author_id = user['userid']
        author_name = user['nickname']
        note_type = item['type']
        df = pd.concat([df, pd.DataFrame({
            'note_id': [note_id],
            'note_link': [f'https://www.xiaohongshu.com/explore/{note_id}'],
            'title': [title],
            'publish_time': [parse_timestamp(item.get('create_time'))],
            'author_id': [author_id],
            'author_name': [author_name],
            'author_link': [f'https://www.xiaohongshu.com/user/profile/{author_id}'],
            'image_list': [json.dumps(get_image_list(item.get('images_list')), ensure_ascii=False)],
            'image_count': [item.get('image_count')],
            'collected_count': [item.get('collected_count')],
            'liked_count': [liked_count],
            'note_type': [note_type],
        })])
    df.drop_duplicates(keep='first', subset=['note_id'], inplace=True)
    df.to_csv(result_file, index=False, encoding='utf-8-sig')
    print(f'has saved {df.shape[0]} notes in file {result_file}')


def do_keyword_crawl(topic_id):
    params = {
        'page_size': '1',
        'sort': 'hot',  # time 时间排序，hot 热度排序
        'page_id': f'{topic_id}',
        'cursor': '',
        'sid': '',
    }
    cur_page = 1

    while True:
        print(f'cur page {cur_page}')
        response = requests.get('https://www.xiaohongshu.com/web_api/sns/v3/page/notes',
                                params=params,
                                headers=headers)

        try:
            resp_json = response.json()
        except requests.exceptions:
            print(traceback.format_exc())
            break
        save_data(resp_json, topic_id)
        cursor = resp_json.get('data', {}).get('cursor', '')

        has_more = resp_json.get('data', {}).get('has_more', '')
        print(f'cursor {cursor} {has_more}')

        if not has_more:
            print('has no more ')
            break

        cur_page += 1
        sleep(slp_tms_per_request)

        params['cursor'] = cursor


if __name__ == '__main__':
    # 修改想要抓取的话题的 ID
    do_keyword_crawl(topic_id='5c3a0b93a1710a00014c1914')
