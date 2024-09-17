package com.cosmetic.gg.service.statistic.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.cosmetic.gg.common.constants.CommonConstant;
import com.cosmetic.gg.common.enums.EStatus;
import com.cosmetic.gg.common.utils.string.StringUtils;
import com.cosmetic.gg.dto.response.StatisticResponse;
import com.cosmetic.gg.dto.response.attribute.SkinTypeResponse;
import com.cosmetic.gg.dto.response.product.BrandResponse;
import com.cosmetic.gg.dto.response.product.ProductResponse;
import com.cosmetic.gg.entity.attribute.SkinType;
import com.cosmetic.gg.entity.product.Brand;
import com.cosmetic.gg.entity.product.Product;
import com.cosmetic.gg.model.product.CategoryModel;
import com.cosmetic.gg.repository.attribute.SkinTypeRepository;
import com.cosmetic.gg.repository.order.OrderItemRepository;
import com.cosmetic.gg.repository.product.BrandRepository;
import com.cosmetic.gg.repository.product.CategoryRepository;
import com.cosmetic.gg.repository.product.FavoriteRepository;
import com.cosmetic.gg.repository.product.ProductRepository;
import com.cosmetic.gg.service.attribute.SkinTypeService;
import com.cosmetic.gg.service.product.BrandService;
import com.cosmetic.gg.service.product.CategoryService;
import com.cosmetic.gg.service.statistic.StatisticService;

@Service
public class StatisticServiceImpl implements StatisticService{

	private static final Logger log = LoggerFactory.getLogger(StatisticServiceImpl.class);
	
	@Autowired
	private ProductRepository productRepository;
	
	@Autowired
	private FavoriteRepository favoriteRepository;
	
	@Autowired
	private OrderItemRepository orderItemRepository;
	
	@Autowired
	private BrandRepository brandRepository;
	
	@Autowired
	private SkinTypeRepository skinTypeRepository;
	
	@Autowired
	private CategoryRepository categoryRepository;
	
	@Autowired
	private BrandService brandService;
	
	@Autowired
	private SkinTypeService skinTypeService;
	
	@Autowired
	private CategoryService categoryService;
	
	@Override
	public Map<String, Object> newest(String keyword, EStatus status, String brandId, String categoryId, String skinTypeId, 
			Float min, Float max, Boolean isDate, Integer pageIndex, Integer pageSize) {
		Map<String, Object> result = new HashMap<>();
		try {
			pageSize = pageSize == 0 ? CommonConstant.DEFAULT_PAGE_SIZE : pageSize;
			pageIndex = pageIndex > 0 ? (pageIndex - 1)*pageSize : 0;
			min = min == -1.0f ? null : min;
			max = max == -1.0f ? null : max;
			if(min != null && max != null) 
			if(min>max)
			return null;
			
			List<ProductResponse> products = new ArrayList<>();
			List<Integer> totalItem = new ArrayList<>();
			if(Boolean.TRUE.equals(StringUtils.isNullOrEmpty(skinTypeId))) {
			List<Object> objects = productRepository.newest(
				Boolean.TRUE.equals(StringUtils.isNullOrEmpty(keyword)) ? null : keyword,
				Objects.isNull(status) ? null : status.name(),
				Boolean.TRUE.equals(StringUtils.isNullOrEmpty(brandId)) ? null : brandId,
				Boolean.TRUE.equals(StringUtils.isNullOrEmpty(categoryId)) ? null : categoryId,
				min, max, 
				Objects.isNull(isDate) ? null : isDate,
				pageIndex, pageSize);
			for(Object rs: objects) {
				if(rs instanceof Object[]) {
					Object[] objArray = (Object[]) rs;
					ProductResponse productResponse = new ProductResponse();
					productResponse.setId((String) objArray[0]);
					productResponse.setName((String) objArray[2]);
					productResponse.setCode((String) objArray[1]);
					productResponse.setPhoto((byte[]) objArray[3]);
					productResponse.setMadeIn((String) objArray[4]);
					
					EStatus statuss = null;
					if(((String) objArray[5]).equalsIgnoreCase("STOCK")) statuss = EStatus.STOCK;
					else if (((String) objArray[5]).equalsIgnoreCase("SOLD_OUT")) statuss = EStatus.SOLD_OUT;
					else statuss = EStatus.HIDDEN;
					productResponse.setStatus(statuss);
					productResponse.setDescription((String) objArray[6]);
					productResponse.setProductionDate(((java.sql.Timestamp) objArray[7]).toLocalDateTime());
					productResponse.setExpirationDate(((java.sql.Timestamp) objArray[8]).toLocalDateTime());
					productResponse.setCategoryId((String) objArray[9]);
					productResponse.setBrandId((String) objArray[10]);
					productResponse.setCategoryName((String) objArray[11]);
					productResponse.setBrandName((String) objArray[12]);
					productResponse.setMinPrice((Float) objArray[13]);
					productResponse.setMaxPrice((Float) objArray[14]);
					
					List<String> skinTypePairs = Arrays.asList(((String) objArray[15]).split(","));
					List<SkinTypeResponse> skinTypeResponses = new ArrayList<>();
					for(String pair: skinTypePairs) {
						String[] parts = pair.split(":");
						if(parts.length == 2) {
							SkinTypeResponse skinTypeResponse = new SkinTypeResponse();
							skinTypeResponse.setId(parts[0]);
							skinTypeResponse.setName(parts[1]);
							skinTypeResponses.add(skinTypeResponse);
						}
					}
					productResponse.setSkinTypes(skinTypeResponses);
					Integer totalFavorite = favoriteRepository.cntProductFavorite((String) objArray[0]);
					productResponse.setTotalFavorite(totalFavorite);
					Integer cnt = productRepository.cntTotalSellByProduct(productResponse.getId());
					productResponse.setTotalSell(cnt);
					products.add(productResponse);
				}
			}
			
			totalItem = productRepository.cntProduct(
				Boolean.TRUE.equals(StringUtils.isNullOrEmpty(keyword)) ? null : keyword,
				Objects.isNull(status) ? EStatus.STOCK.name() : status.name(),
				Boolean.TRUE.equals(StringUtils.isNullOrEmpty(brandId)) ? null : brandId,
				Boolean.TRUE.equals(StringUtils.isNullOrEmpty(categoryId)) ? null : categoryId,
				min, max,
				Objects.isNull(isDate) ? null : isDate);
			}else {
			
			List<Object> objects = productRepository.searchBySkinTypeNewest(
			Boolean.TRUE.equals(StringUtils.isNullOrEmpty(keyword)) ? null : keyword,
			Objects.isNull(status) ? EStatus.STOCK.name() : status.name(),
			Boolean.TRUE.equals(StringUtils.isNullOrEmpty(brandId)) ? null : brandId,
			Boolean.TRUE.equals(StringUtils.isNullOrEmpty(categoryId)) ? null : categoryId,
			Boolean.TRUE.equals(StringUtils.isNullOrEmpty(skinTypeId)) ? null : skinTypeId,
			min, max, 
			Objects.isNull(isDate) ? null : isDate,
			pageIndex, pageSize);
			for(Object rs: objects) {
				if(rs instanceof Object[]) {
					Object[] objArray = (Object[]) rs;
					ProductResponse productResponse = new ProductResponse();
					productResponse.setId((String) objArray[0]);
					productResponse.setName((String) objArray[2]);
					productResponse.setCode((String) objArray[1]);
					productResponse.setPhoto((byte[]) objArray[3]);
					productResponse.setMadeIn((String) objArray[4]);
					
					EStatus statuss = null;
					if(((String) objArray[5]).equalsIgnoreCase("STOCK")) statuss = EStatus.STOCK;
					else if (((String) objArray[5]).equalsIgnoreCase("SOLD_OUT")) statuss = EStatus.SOLD_OUT;
					else statuss = EStatus.HIDDEN;
					productResponse.setStatus(statuss);
					productResponse.setDescription((String) objArray[6]);
					productResponse.setProductionDate(((java.sql.Timestamp) objArray[7]).toLocalDateTime());
					productResponse.setExpirationDate(((java.sql.Timestamp) objArray[8]).toLocalDateTime());
					productResponse.setCategoryId((String) objArray[9]);
					productResponse.setBrandId((String) objArray[10]);
					productResponse.setCategoryName((String) objArray[11]);
					productResponse.setBrandName((String) objArray[12]);
					productResponse.setMinPrice((Float) objArray[13]);
					productResponse.setMaxPrice((Float) objArray[14]);
					
					List<String> skinTypePairs = Arrays.asList(((String) objArray[15]).split(","));
					List<SkinTypeResponse> skinTypeResponses = new ArrayList<>();
					for(String pair: skinTypePairs) {
						String[] parts = pair.split(":");
						if(parts.length == 2) {
							SkinTypeResponse skinTypeResponse = new SkinTypeResponse();
							skinTypeResponse.setId(parts[0]);
							skinTypeResponse.setName(parts[1]);
							skinTypeResponses.add(skinTypeResponse);
						}
					}
					productResponse.setSkinTypes(skinTypeResponses);
					Integer totalFavorite = favoriteRepository.cntProductFavorite((String) objArray[0]);
					productResponse.setTotalFavorite(totalFavorite);
					products.add(productResponse);
				}
			}
			
			totalItem = productRepository.cntProductBySkinType(
				Boolean.TRUE.equals(StringUtils.isNullOrEmpty(keyword)) ? null : keyword,
				Objects.isNull(status) ? EStatus.STOCK.name() : status.name(),
				Boolean.TRUE.equals(StringUtils.isNullOrEmpty(brandId)) ? null : brandId,
				Boolean.TRUE.equals(StringUtils.isNullOrEmpty(categoryId)) ? null : categoryId,
				Boolean.TRUE.equals(StringUtils.isNullOrEmpty(skinTypeId)) ? null : skinTypeId,
				min, max,
				Objects.isNull(isDate) ? null : isDate);
			}
		result.put("data", products);
		result.put("totalItem", totalItem.size());
		}catch(Exception ex) {
			result.put("data", new ArrayList<Product>());
			result.put("totalItem", 0);
			log.error("Error while searching product newest", ex.getCause());
		}
		return result;
	}
	
	@Override
	public List<StatisticResponse> topSellBrandGraph(EStatus status) {
		List<StatisticResponse> result = new ArrayList<>();
		try {
			List<Brand> brands = brandService.search(null, status);
			for(Brand brand: brands) {
				StatisticResponse response = new StatisticResponse();
				Integer cnt = productRepository.cntTotalSellByBrand(brand.getId());
				if(cnt == null) cnt =0;
				response.setName(brand.getName());
				response.setValue(cnt);
				result.add(response);
			}
		}catch(Exception ex) {
			log.error("Error while searching product newest", ex.getCause());
		}
		return result;
	}
	
//	@Override
//	public List<BrandResponse> topSellBrand(EStatus status) {
//		List<BrandResponse> result = new ArrayList<>();
//		try {
//			List<Brand> brands = brandService.search(null, status);
//			for(Brand brand: brands) {
//				BrandResponse response = new BrandResponse();
//				Integer cnt = productRepository.cntTotalSellByBrand(brand.getId());
//				if(cnt == null) cnt =0;
//				response.setId(brand.getId());
//				response.setCode(brand.getCode());
//				response.setName(brand.getName());
//				response.setCountry(brand.getCountry());
//				response.setLogo(brand.getLogo());
//				response.setValue(cnt);
//				result.add(response);
//			}
//		}catch(Exception ex) {
//			log.error("Error while searching product newest", ex.getCause());
//		}
//		return result;
//	}
	
	@Override
	public List<StatisticResponse> topSellSkinType() {
		List<StatisticResponse> result = new ArrayList<>();
		try {
			List<SkinType> skintypes = skinTypeService.search();
			for(SkinType skintype: skintypes) {
				StatisticResponse response = new StatisticResponse();
				Integer cnt = productRepository.cntTotalSellBySkinType(skintype.getId());
				if(cnt == null) cnt =0;
				response.setName(skintype.getName());
				response.setValue(cnt);
				result.add(response);
			}
		}catch(Exception ex) {
			log.error("Error while searching product newest", ex.getCause());
		}
		return result;
	}
	
	public List<StatisticResponse> topSellCategoryRoot() {
		List<StatisticResponse> result = new ArrayList<>();
		try {
//			List<CategoryModel> categories = categoryService.root();
//			for(CategoryModel category: categories) {
//				StatisticResponse response = new StatisticResponse();
//				Integer cnt = productRepository.cntTotalSellBySkinType(skintype.getId());
//				if(cnt == null) cnt =0;
//				response.setName(skintype.getName());
//				response.setValue(cnt);
//				result.add(response);
//			}
		}catch(Exception ex) {
			log.error("Error while searching product newest", ex.getCause());
		}
		return result;
	}
	
	@Override
	public List<StatisticResponse> topSellCategory(String categoryRootId) {
		List<StatisticResponse> result = new ArrayList<>();
		try {
//			List<CategoryModel> categories = categoryService.children(categoryRootId, false);
//			for(CategoryModel category: categories) {
//				StatisticResponse response = new StatisticResponse();
//				Integer cnt = productRepository.cntTotalSellByCategory(category.getId());
//				if(cnt == null) cnt =0;
//				response.setName(category.getName());
//				response.setValue(cnt);
//				result.add(response);
//			}
		}catch(Exception ex) {
			log.error("Error while searching product newest", ex.getCause());
		}
		return result;
	}
}
