FROM tboquet/bokehviz

RUN pip install xarray redis

#ADD geoflix.py /app/geoflix.py
EXPOSE 8889

CMD bokeh serve --log-level=debug --show --port=8889 --allow-websocket-origin="*" /app/geoflix.py
