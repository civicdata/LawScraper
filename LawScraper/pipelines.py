# -*- coding: utf-8 -*-

# Define your item pipelines here
#
# Don't forget to add your pipeline to the ITEM_PIPELINES setting
# See: http://doc.scrapy.org/en/latest/topics/item-pipeline.html

import time
import scrapy
import logging
from scrapy.pipelines.files import FilesPipeline
from scrapy.utils.project import get_project_settings

logger = logging.getLogger()


class StatePDFPipeline(FilesPipeline):
    def item_completed(self, results, item, info):
        if not(results[0][0]):
            item['failure'] = 'PDF Download'
            return item

        file_info = results[0][1]
        item['pdf_path'] = get_project_settings().get('FILES_STORE') + file_info['path']
        item['pdf_md5'] = file_info['checksum']
        item['download_time'] = time.strftime("%Y-%m-%d %H:%M:%S")
        return item


