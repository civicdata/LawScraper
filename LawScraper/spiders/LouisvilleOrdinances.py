# -*- coding: utf-8 -*-
import scrapy


class LouisvilleordinancesSpider(scrapy.Spider):
    name = "LouisvilleOrdinances"
    allowed_domains = ["http://library.amlegal.com/nxt/gateway.dll/Kentucky/loukymetro/"]
    start_urls = (
        'http://www.http://library.amlegal.com/nxt/gateway.dll/Kentucky/loukymetro//',
    )

    def parse(self, response):
        pass
