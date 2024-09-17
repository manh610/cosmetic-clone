package com.cosmetic.gg.controller;

import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

import com.cosmetic.gg.common.constants.CommonConstant;
import com.cosmetic.gg.common.dto.Response;
import com.cosmetic.gg.common.enums.EStatus;
import com.cosmetic.gg.common.enums.ErrorCode;
import com.cosmetic.gg.common.exception.GlobalException;
import com.cosmetic.gg.dto.response.StatisticResponse;
import com.cosmetic.gg.entity.product.Brand;
import com.cosmetic.gg.service.statistic.StatisticService;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@RestController
@RequestMapping(path = "/api/v1/statistic")
@AllArgsConstructor
@CrossOrigin
public class StatisticController {
	private final StatisticService statisticService;
	
	@GetMapping(value = "/newest")
	@ResponseStatus(value = HttpStatus.OK)
	public ResponseEntity<Response<?>> newest(
			@RequestParam(value = "keyword", required = false, defaultValue = "") String keyword,
			@RequestParam(value = "status", required = false, defaultValue = "") EStatus status,
			@RequestParam(value = "brandId", required = false, defaultValue = "") String brandId,
			@RequestParam(value = "categoryId", required = false, defaultValue = "") String categoryId,
			@RequestParam(value = "skinTypeId", required = false, defaultValue = "") String skinTypeId,
			@RequestParam(value = "min", required = false, defaultValue = "-1.0f") float min,
			@RequestParam(value = "max", required = false, defaultValue = "-1.0f") float max,
			@RequestParam(value = "isDate", required = false, defaultValue = "") Boolean isDate,
			@RequestParam(value = "pageIndex", required = false, defaultValue = "1") int pageIndex,
			@RequestParam(value = "pageSize", required = false, defaultValue = "10") int pageSize) {
		String transId = UUID.randomUUID().toString();
		try {
			Map<String, Object> result = statisticService.newest(keyword, status, brandId, categoryId, skinTypeId, min, max, 
					isDate, pageIndex, pageSize);
			Response<Object> response = new Response<>().success(
					transId,
					Integer.parseInt(result.get("totalItem").toString()),
					result.get("data")
			);
			return new ResponseEntity<>(
					response,
					HttpStatus.OK
			);
		}catch(Exception ex) {
			log.error(CommonConstant.EXCEPTION, ex.getCause());
			throw GlobalException.builder(ErrorCode.EXCEPTION, transId);
		}
	}
	
	@GetMapping(value = "/brand")
	@ResponseStatus(value = HttpStatus.OK)
	public ResponseEntity<Response<?>> topSellBrand(
			@RequestParam(value = "status", required = false, defaultValue = "") EStatus status) {
		String transId = UUID.randomUUID().toString();
		try {
			List<StatisticResponse> brands = statisticService.topSellBrandGraph(status);
			return new ResponseEntity<>(
		        new Response<List<StatisticResponse>>().success(
		          transId,
		          brands.size(),
		          brands
		        ),
		        HttpStatus.OK
		      );
		}catch(Exception ex) {
			log.error(CommonConstant.EXCEPTION, ex.getCause());
			throw GlobalException.builder(ErrorCode.EXCEPTION, transId);
		}
	}
	
	@GetMapping(value = "/skin-type")
	@ResponseStatus(value = HttpStatus.OK)
	public ResponseEntity<Response<?>> topSellSkinType() {
		String transId = UUID.randomUUID().toString();
		try {
			List<StatisticResponse> skintypes = statisticService.topSellSkinType();
			return new ResponseEntity<>(
		        new Response<List<StatisticResponse>>().success(
		          transId,
		          skintypes.size(),
		          skintypes
		        ),
		        HttpStatus.OK
		      );
		}catch(Exception ex) {
			log.error(CommonConstant.EXCEPTION, ex.getCause());
			throw GlobalException.builder(ErrorCode.EXCEPTION, transId);
		}
	}
	
	@GetMapping(value = "/category")
	@ResponseStatus(value = HttpStatus.OK)
	public ResponseEntity<Response<?>> topSellCategory(
			@RequestParam(value = "categoryRootId", required = false, defaultValue = "") String categoryRootId) {
		String transId = UUID.randomUUID().toString();
		try {
			List<StatisticResponse> categories = statisticService.topSellCategory(categoryRootId);
			return new ResponseEntity<>(
		        new Response<List<StatisticResponse>>().success(
		          transId,
		          categories.size(),
		          categories
		        ),
		        HttpStatus.OK
		      );
		}catch(Exception ex) {
			log.error(CommonConstant.EXCEPTION, ex.getCause());
			throw GlobalException.builder(ErrorCode.EXCEPTION, transId);
		}
	}
}
